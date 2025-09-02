//! Hand-written Recursive Descent Parser for Lua
//!
//! This parser implements a complete Lua 5.4 parser using recursive descent
//! with custom combinator macros for clean and efficient parsing.

use crate::ast::*;
use crate::error::{ParseError, Span};
use crate::grammar::{BinaryOp, UnaryOp};
use crate::lexer::{Lexer, Token, TokenType};
use crate::node::Node;
use bumpalo::Bump;

/// Parser state with token stream and arena allocator
pub(crate) struct Parser<'arena, 'input: 'arena> {
    lexer: Lexer<'input>,
    peeked: Option<Result<Token<'input>, ParseError>>,
    previous_span: Span,
    #[allow(dead_code)]
    arena: &'arena Bump,
    builder: AstBuilder<'arena>,
    errors: Vec<ParseError>,
}

impl<'arena, 'input: 'arena> Parser<'arena, 'input> {
    /// Create a new parser with the given input and arena
    pub(crate) fn new(input: &'input str, arena: &'arena Bump) -> Self {
        let lexer = Lexer::new(input);
        let initial_pos = lexer.current_position();
        Self {
            lexer,
            peeked: None,
            previous_span: Span::single(initial_pos),
            arena,
            builder: AstBuilder::new(arena),
            errors: Vec::new(),
        }
    }

    /// Parse the input into a complete AST root Block and return collected errors.
    /// This method is intended for internal use by the `lib.rs`'s `parse` function.
    pub(crate) fn parse_into_root_block(&mut self) -> (&'arena Block<'arena>, Vec<ParseError>) {
        let chunk = self.parse_chunk();
        (chunk, self.errors.drain(..).collect())
    }

    /// Parse a chunk (top-level block)
    fn parse_chunk(&mut self) -> &'arena Block<'arena> {
        self.parse_block()
    }

    fn skip_trivia(&mut self) {
        loop {
            if matches!(
                self.peek(),
                Some(Ok(Token {
                    token_type:
                        TokenType::Whitespace(_)
                            | TokenType::LineComment(_)
                            | TokenType::BlockComment(_),
                    ..
                }))
            ) {
                self.advance();
            } else {
                break;
            }
        }
    }

    // === Error and token helpers ===
    fn found_token_description(&mut self) -> String {
        self.peek()
            .map(|t| format!("{:?}", t.as_ref().unwrap().token_type))
            .unwrap_or_else(|| "EOF".to_string())
    }

    fn push_unexpected(&mut self, expected: &str) {
        let span = self.current_span();
        let found = self.found_token_description();
        self.errors.push(ParseError::UnexpectedToken {
            expected: expected.to_string(),
            found,
            span,
        });
    }

    fn push_invalid(&mut self, message: impl Into<String>) {
        let span = self.current_span();
        self.errors.push(ParseError::InvalidSyntax {
            message: message.into(),
            span,
        });
    }

    /// Expect a specific token kind (by discriminant). If present, consumes it and returns true.
    /// Otherwise, reports an UnexpectedToken and returns false.
    fn expect(&mut self, token_type: &TokenType, expected: &str) -> bool {
        if self.check(token_type) {
            self.advance();
            true
        } else {
            self.push_unexpected(expected);
            false
        }
    }

    /// Parse a block
    fn parse_block(&mut self) -> &'arena Block<'arena> {
        let start_span = self.current_span();
        let mut statements = Vec::new();

        // Parse statements until we hit a block terminator or EOF
        while !self.is_at_end() {
            self.skip_trivia();
            if self.is_block_terminator() {
                break;
            }

            if let Some(stmt) = self.try_parse_statement() {
                statements.push(stmt);
            } else {
                // If we couldn't parse a statement and we're not at a terminator,
                // this is a syntax error
                if !self.is_block_terminator() && !self.is_at_end() {
                    let found = self
                        .peek()
                        .map(|t| format!("{:?}", t.as_ref().unwrap().token_type))
                        .unwrap_or_else(|| "EOF".to_string());
                    let span = self.current_span();
                    self.errors.push(ParseError::UnexpectedToken {
                        expected: "statement".to_string(),
                        found,
                        span,
                    });
                    // Recovery: skip to next statement
                    self.synchronize();
                }
            }
        }

        // Check for optional return statement
        let return_stmt = if self.check(&TokenType::Return) {
            self.parse_return_statement()
        } else {
            None
        };

        let end_span = self.previous_span;
        let span = Span::new(start_span.start, end_span.end);

        self.builder.alloc(Block {
            statements: self.builder.alloc_slice(&statements),
            return_stmt,
            span,
        })
    }

    /// Try to parse a statement, returning None for empty statements
    fn try_parse_statement(&mut self) -> Option<Statement<'arena>> {
        self.skip_trivia();
        // Handle empty statements (just semicolons)
        if self.check(&TokenType::Semicolon) {
            let token = self.advance();
            return Some(Statement::Empty { span: token.span });
        }

        // Parse different statement types
        let peeked_token_type = self.peek().map(|r| r.as_ref().map(|t| &t.token_type));
        match peeked_token_type {
            Some(Ok(TokenType::Local)) => {
                let local_span_start = self.advance().span.start; // consume 'local'
                if self.check(&TokenType::Function) {
                    // local function name funcbody
                    self.advance(); // consume 'function'

                    // Allow trivia between 'function' and the identifier
                    self.skip_trivia();
                    let name = if let Some(Ok(Token {
                        token_type: TokenType::Identifier(name),
                        ..
                    })) = self.peek()
                    {
                        let name_val = *name;
                        self.advance(); // consume identifier
                        name_val
                    } else {
                        let span = self.current_span();
                        self.errors.push(ParseError::InvalidSyntax {
                            message: "Expected function name after 'local function'".to_string(),
                            span,
                        });
                        return None;
                    };

                    let body = self.parse_function_body();

                    let span = Span::new(local_span_start, body.span.end);

                    return Some(Statement::LocalFunction {
                        name: self.builder.alloc_str(name),
                        body: self.builder.alloc(body),
                        span,
                    });
                } else {
                    // local attnamelist ['=' explist]
                    let local_token_span = self.previous_span; // span of 'local'

                    let mut attributed_names: Vec<AttributedName> = Vec::new();
                    loop {
                        // Allow trivia before each name
                        self.skip_trivia();

                        // Name
                        let (name, name_span_start) = if let Some(Ok(Token {
                            token_type: TokenType::Identifier(name_val),
                            ..
                        })) = self.peek()
                        {
                            let name_val = *name_val;
                            let t = self.advance();
                            (name_val, t.span.start)
                        } else {
                            let span = self.current_span();
                            self.errors.push(ParseError::InvalidSyntax {
                                message: "Expected identifier".to_string(),
                                span,
                            });
                            return None;
                        };

                        // Optional attribute: '<' Name '>'
                        let mut attribute: Option<&'arena str> = None;
                        let mut name_span_end = self.previous_span.end;
                        if self.check(&TokenType::Lt) {
                            self.advance(); // consume '<'
                            // consume attribute identifier token
                            let t = self.advance();
                            match t.token_type {
                                TokenType::Identifier(attr_name) => {
                                    attribute = Some(self.builder.alloc_str(attr_name));
                                }
                                _ => {
                                    let span = self.current_span();
                                    self.errors.push(ParseError::InvalidSyntax {
                                        message: "Expected attribute name after '<'".to_string(),
                                        span,
                                    });
                                    return None;
                                }
                            }
                            if !self.check(&TokenType::Gt) {
                                let span = self.current_span();
                                self.errors.push(ParseError::InvalidSyntax {
                                    message: "Expected '>' after attribute name".to_string(),
                                    span,
                                });
                                return None;
                            }
                            name_span_end = self.advance().span.end; // consume '>' and extend span
                        }

                        let attributed_name = AttributedName {
                            name: self.builder.alloc_str(name),
                            attribute,
                            span: Span::new(name_span_start, name_span_end),
                        };
                        attributed_names.push(attributed_name);

                        if self.check(&TokenType::Comma) {
                            self.advance(); // consume ',' and continue for next name
                            // Skip possible trivia after comma
                            self.skip_trivia();
                            continue;
                        }

                        break;
                    }

                    // Optional assignment
                    let expressions = if self.check(&TokenType::Assign) {
                        self.advance(); // consume '='
                        let expr_list = self.parse_expression_list();
                        Some(expr_list)
                    } else {
                        None
                    };

                    let end_span = self.previous_span;
                    let span = Span::new(local_token_span.start, end_span.end);

                    let names = self.builder.alloc_slice(&attributed_names);

                    return Some(Statement::LocalVariables {
                        names,
                        expressions,
                        span,
                    });
                }
            }
            Some(Ok(TokenType::Function)) => {
                // function funcname funcbody
                self.parse_function_definition()
            }
            Some(Ok(TokenType::If)) => {
                // if exp then block {elseif exp then block} [else block] end
                self.parse_if_statement()
            }
            Some(Ok(TokenType::While)) => {
                // while exp do block end
                self.parse_while_statement()
            }
            Some(Ok(TokenType::For)) => {
                // for ... parsing
                self.parse_for_statement()
            }
            Some(Ok(TokenType::Repeat)) => {
                // repeat block until exp
                self.parse_repeat_statement()
            }
            Some(Ok(TokenType::Do)) => {
                // do block end
                self.parse_do_statement()
            }
            Some(Ok(TokenType::Break)) => {
                let token = self.advance();
                return Some(Statement::Break { span: token.span });
            }
            Some(Ok(TokenType::Goto)) => {
                // goto Name
                let start_span = self.advance().span; // consume 'goto'
                self.skip_trivia();
                let name = if let Some(Ok(Token {
                    token_type: TokenType::Identifier(name),
                    ..
                })) = self.peek()
                {
                    let ident = *name;
                    self.advance(); // consume identifier
                    ident
                } else {
                    let span = self.current_span();
                    self.errors.push(ParseError::InvalidSyntax {
                        message: "Expected label name after 'goto'".to_string(),
                        span,
                    });
                    return None;
                };
                let span = Span::new(start_span.start, self.previous_span.end);
                return Some(Statement::Goto {
                    label: self.builder.alloc_str(name),
                    span,
                });
            }
            Some(Ok(TokenType::DoubleColon)) => {
                // ::label::
                let start_span = self.advance().span; // consume '::'

                let name = if let Some(Ok(Token {
                    token_type: TokenType::Identifier(name),
                    ..
                })) = self.peek()
                {
                    let ident = *name;
                    self.advance(); // consume identifier
                    ident
                } else {
                    let span = self.current_span();
                    self.errors.push(ParseError::InvalidSyntax {
                        message: "Expected label name after '::'".to_string(),
                        span,
                    });
                    return None;
                };

                if !self.check(&TokenType::DoubleColon) {
                    let span = self.current_span();
                    self.errors.push(ParseError::InvalidSyntax {
                        message: "Expected closing '::' after label name".to_string(),
                        span,
                    });
                    return None;
                }
                let end_span = self.advance().span; // consume closing '::'

                let span = Span::new(start_span.start, end_span.end);
                return Some(Statement::Label {
                    name: self.builder.alloc_str(name),
                    span,
                });
            }
            Some(Ok(TokenType::Return)) => {
                // Return statements are handled in parse_block, not here
                return None;
            }
            _ => {
                // At this point, it must be an assignment or a function call.
                // Both start with a prefix expression.
                if let Some(expr) = self.try_parse_prefix_expression() {
                    match expr {
                        Expression::Variable { .. } => {
                            // A variable can start an assignment if followed by '=' or ',' (varlist)
                            if self.check(&TokenType::Assign) || self.check(&TokenType::Comma) {
                                let mut variables = Vec::new();
                                if let Expression::Variable { var, .. } = expr {
                                    variables.push(var);
                                }
                                while self.check(&TokenType::Comma) {
                                    self.advance();
                                    if let Some(next_expr) = self.try_parse_prefix_expression() {
                                        if let Expression::Variable { var, .. } = next_expr {
                                            variables.push(var);
                                        } else {
                                            self.push_invalid("Invalid left-hand side of assignment");
                                            return None;
                                        }
                                    } else {
                                        self.push_invalid("Expected variable after ','");
                                        return None;
                                    }
                                }
                                return self.parse_assignment_statement(variables);
                            } else {
                                // Standalone variable as a statement is invalid in Lua
                                self.push_unexpected("'=' or function call arguments");
                                return None;
                            }
                        }
                        Expression::FunctionCall { .. } => {
                            let span = expr.span();
                            return Some(Statement::Expression {
                                expression: self.builder.alloc(expr),
                                span,
                            });
                        }
                        _ => {
                            self.push_unexpected("assignment or function call");
                            return None;
                        }
                    }
                }
                self.push_unexpected("assignment or function call");
                return None;
            }
        }
    }

    /// Parse a function definition statement
    fn parse_function_definition(&mut self) -> Option<Statement<'arena>> {
        let start_token = self.advance(); // consume 'function'

        let name = self.parse_function_name();
        let body = self.parse_function_body();

        let span = Span::new(start_token.span.start, body.span.end);

        Some(Statement::FunctionDef {
            name: self.builder.alloc(name),
            body: self.builder.alloc(body),
            span,
        })
    }

    /// Parse a function name (e.g., `foo.bar:baz`)
    fn parse_function_name(&mut self) -> FunctionName<'arena> {
        let start_span = self.current_span();

        let base = if let Some(Ok(Token {
            token_type: TokenType::Identifier(name),
            ..
        })) = self.peek()
        {
            *name
        } else {
            let found = self
                .peek()
                .map(|t| format!("{:?}", t.as_ref().unwrap().token_type))
                .unwrap_or_else(|| "EOF".to_string());
            self.errors.push(ParseError::UnexpectedToken {
                expected: "identifier".to_string(),
                found,
                span: start_span,
            });
            return FunctionName {
                base: self.builder.alloc_str(""),
                fields: self.builder.alloc_slice(&[]),
                method: None,
                span: start_span,
            };
        };
        self.advance();

        let mut fields = Vec::new();
        while self.check(&TokenType::Dot) {
            self.advance(); // consume '.'
            if let Some(Ok(Token {
                token_type: TokenType::Identifier(field),
                ..
            })) = self.peek()
            {
                fields.push(*field);
                self.advance();
            } else {
                let span = self.current_span();
                self.errors.push(ParseError::InvalidSyntax {
                    message: "Expected field name after '.'".to_string(),
                    span,
                });
                return FunctionName {
                    base: self.builder.alloc_str(""),
                    fields: self.builder.alloc_slice(&[]),
                    method: None,
                    span: start_span,
                };
            }
        }

        let method = if self.check(&TokenType::Colon) {
            self.advance(); // consume ':'
            if let Some(Ok(Token {
                token_type: TokenType::Identifier(method_name),
                ..
            })) = self.peek()
            {
                let method_name = *method_name;
                self.advance(); // consume method name
                Some(self.builder.alloc_str(method_name))
            } else {
                let span = self.current_span();
                self.errors.push(ParseError::InvalidSyntax {
                    message: "Expected method name after ':'".to_string(),
                    span,
                });
                Some(self.builder.alloc_str(""))
            }
        } else {
            None
        };

        let span = Span::new(start_span.start, self.previous_span.end);

        FunctionName {
            base: self.builder.alloc_str(base),
            fields: self.builder.alloc_slice(&fields),
            method,
            span,
        }
    }

    /// Parse a function body `( [parlist] ) block end`
    fn parse_function_body(&mut self) -> FunctionBody<'arena> {
        let start_span = self.current_span();

        if !self.check(&TokenType::LeftParen) {
            self.push_unexpected("'('");
            // Recovery: assume no parameters and parse a body anyway
            let body = self.parse_block();
            if self.check(&TokenType::End) {
                self.advance();
            }
            let end_span = self.previous_span;
            return FunctionBody {
                parameters: self.builder.alloc_slice(&[]),
                is_vararg: false,
                body,
                span: Span::new(start_span.start, end_span.end),
            };
        }
        self.advance(); // consume '('

        let mut parameters = Vec::new();
        let mut is_vararg = false;
        if !self.check(&TokenType::RightParen) {
            loop {
                self.skip_trivia(); // Handle trivia before parameter
                if let Some(Ok(Token {
                    token_type: TokenType::Identifier(name),
                    ..
                })) = self.peek()
                {
                    parameters.push(*name);
                    self.advance();
                } else if self.check(&TokenType::Ellipsis) {
                    is_vararg = true;
                    self.advance();
                    break;
                } else {
                    // An error here doesn't mean we should stop parsing the function.
                    // We'll report it and then break out of the parameter loop.
                    let span = self.current_span();
                    self.errors.push(ParseError::InvalidSyntax {
                        message: "Expected parameter name or '...'".to_string(),
                        span,
                    });
                    break;
                }

                if self.check(&TokenType::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        if !self.expect(&TokenType::RightParen, "')' after function parameters") {
            // proceed
        }

        let body = self.parse_block();

        if !self.expect(&TokenType::End, "'end' after function body") {
            // proceed
        }

        let span = Span::new(start_span.start, self.previous_span.end);

        FunctionBody {
            parameters: self.builder.alloc_slice(&parameters),
            is_vararg,
            body,
            span,
        }
    }

    /// Parse a for statement (numeric or generic)
    fn parse_for_statement(&mut self) -> Option<Statement<'arena>> {
        let start_span = self.advance().span; // consume 'for'

        self.skip_trivia(); // Skip whitespace after 'for'
        let var_name = if let Some(Ok(Token {
            token_type: TokenType::Identifier(name),
            ..
        })) = self.peek()
        {
            *name
        } else {
            let span = self.current_span();
            self.errors.push(ParseError::InvalidSyntax {
                message: "Expected identifier after 'for'".to_string(),
                span,
            });
            return None;
        };
        self.advance(); // consume identifier

        if self.check(&TokenType::Assign) {
            // Numeric for loop
            self.advance(); // consume '='
            let start = if let Some(expr) = self.try_parse_expression() {
                expr
            } else {
                let span = self.current_span();
                let found = self
                    .peek()
                    .map(|t| format!("{:?}", t.as_ref().unwrap().token_type))
                    .unwrap_or_else(|| "EOF".to_string());
                self.errors.push(ParseError::UnexpectedToken {
                    expected: "expression".to_string(),
                    found,
                    span,
                });
                return None;
            };

            if !self.check(&TokenType::Comma) {
                let span = self.current_span();
                let found = self
                    .peek()
                    .map(|t| format!("{:?}", t.as_ref().unwrap().token_type))
                    .unwrap_or_else(|| "EOF".to_string());
                self.errors.push(ParseError::UnexpectedToken {
                    expected: "','".to_string(),
                    found,
                    span,
                });
                return None;
            }
            self.advance(); // consume comma
            let end = if let Some(expr) = self.try_parse_expression() {
                expr
            } else {
                let span = self.current_span();
                let found = self
                    .peek()
                    .map(|t| format!("{:?}", t.as_ref().unwrap().token_type))
                    .unwrap_or_else(|| "EOF".to_string());
                self.errors.push(ParseError::UnexpectedToken {
                    expected: "expression".to_string(),
                    found,
                    span,
                });
                return None;
            };

            let step = if self.check(&TokenType::Comma) {
                self.advance(); // consume comma
                if let Some(expr) = self.try_parse_expression() {
                    Some(expr)
                } else {
                    let span = self.current_span();
                    let found = self
                        .peek()
                        .map(|t| format!("{:?}", t.as_ref().unwrap().token_type))
                        .unwrap_or_else(|| "EOF".to_string());
                    self.errors.push(ParseError::UnexpectedToken {
                        expected: "expression".to_string(),
                        found,
                        span,
                    });
                    return None;
                }
            } else {
                None
            };

            if !self.expect(&TokenType::Do, "'do'") {
                return None;
            }

            let body = self.parse_block();

            if !self.expect(&TokenType::End, "'end'") {
                return None;
            }
            let end_span = self.previous_span; // consumed 'end'

            let span = Span::new(start_span.start, end_span.end);

            Some(Statement::NumericFor {
                variable: self.builder.alloc_str(var_name),
                start: self.builder.alloc(start),
                end: self.builder.alloc(end),
                step: step.map(|s| self.builder.alloc(s) as &_),
                body,
                span,
            })
        } else {
            // Generic for loop: for namelist in explist do block end
            let mut namelist = vec![var_name];
            while self.check(&TokenType::Comma) {
                self.advance(); // consume comma
                self.skip_trivia();
                if let Some(Ok(Token {
                    token_type: TokenType::Identifier(name),
                    ..
                })) = self.peek()
                {
                    namelist.push(*name);
                    self.advance();
                } else {
                    let span = self.current_span();
                    self.errors.push(ParseError::InvalidSyntax {
                        message: "Expected identifier in name list".to_string(),
                        span,
                    });
                    return None;
                }
            }

            if !self.check(&TokenType::In) {
                let span = self.current_span();
                let found = self
                    .peek()
                    .map(|t| format!("{:?}", t.as_ref().unwrap().token_type))
                    .unwrap_or_else(|| "EOF".to_string());
                self.errors.push(ParseError::UnexpectedToken {
                    expected: "'in'".to_string(),
                    found,
                    span,
                });
                return None;
            }
            self.advance(); // consume 'in'

            let expressions = self.parse_expression_list();

            if !self.check(&TokenType::Do) {
                let span = self.current_span();
                let found = self
                    .peek()
                    .map(|t| format!("{:?}", t.as_ref().unwrap().token_type))
                    .unwrap_or_else(|| "EOF".to_string());
                self.errors.push(ParseError::UnexpectedToken {
                    expected: "'do'".to_string(),
                    found,
                    span,
                });
                return None;
            }
            self.advance(); // consume 'do'

            let body = self.parse_block();

            if !self.check(&TokenType::End) {
                let span = self.current_span();
                let found = self
                    .peek()
                    .map(|t| format!("{:?}", t.as_ref().unwrap().token_type))
                    .unwrap_or_else(|| "EOF".to_string());
                self.errors.push(ParseError::UnexpectedToken {
                    expected: "'end'".to_string(),
                    found,
                    span,
                });
                return None;
            }
            let end_span = self.advance().span; // consume 'end'

            let span = Span::new(start_span.start, end_span.end);

            Some(Statement::GenericFor {
                variables: self.builder.alloc_slice(&namelist),
                expressions,
                body,
                span,
            })
        }
    }

    /// Parse a do-end block
    fn parse_do_statement(&mut self) -> Option<Statement<'arena>> {
        let start_span = self.advance().span; // consume 'do'

        let block = self.parse_block();

        if !self.expect(&TokenType::End, "'end'") {
            return None;
        }
        let end_span = self.previous_span; // consumed 'end'

        let span = Span::new(start_span.start, end_span.end);

        Some(Statement::Do { block, span })
    }

    /// Parse a repeat-until statement
    fn parse_repeat_statement(&mut self) -> Option<Statement<'arena>> {
        let start_span = self.advance().span; // consume 'repeat'

        let body = self.parse_block();

        if !self.check(&TokenType::Until) {
            let span = self.current_span();
            let found = self
                .peek()
                .map(|t| format!("{:?}", t.as_ref().unwrap().token_type))
                .unwrap_or_else(|| "EOF".to_string());
            self.errors.push(ParseError::UnexpectedToken {
                expected: "'until'".to_string(),
                found,
                span,
            });
            return None;
        }
        self.advance(); // consume 'until'
        let condition = if let Some(expr) = self.try_parse_expression() {
            expr
        } else {
            let span = self.current_span();
            let found = self
                .peek()
                .map(|t| format!("{:?}", t.as_ref().unwrap().token_type))
                .unwrap_or_else(|| "EOF".to_string());
            self.errors.push(ParseError::UnexpectedToken {
                expected: "condition expression".to_string(),
                found,
                span,
            });
            return None;
        };

        let end_span = self.previous_span;
        let span = Span::new(start_span.start, end_span.end);

        Some(Statement::Repeat {
            body,
            condition: self.builder.alloc(condition),
            span,
        })
    }

    /// Parse a while statement
    fn parse_while_statement(&mut self) -> Option<Statement<'arena>> {
        let start_span = self.advance().span; // consume 'while'
        let condition = if let Some(expr) = self.try_parse_expression() {
            expr
        } else {
            let span = self.current_span();
            let found = self
                .peek()
                .map(|t| format!("{:?}", t.as_ref().unwrap().token_type))
                .unwrap_or_else(|| "EOF".to_string());
            self.errors.push(ParseError::UnexpectedToken {
                expected: "condition expression".to_string(),
                found,
                span,
            });
            return None;
        };

        if !self.expect(&TokenType::Do, "'do'") {
            return None;
        }

        let body = self.parse_block();

        if !self.expect(&TokenType::End, "'end'") {
            return None;
        }
        let end_span = self.previous_span; // consumed 'end'

        let span = Span::new(start_span.start, end_span.end);

        Some(Statement::While {
            condition: self.builder.alloc(condition),
            body,
            span,
        })
    }

    /// Parse an if statement
    fn parse_if_statement(&mut self) -> Option<Statement<'arena>> {
        let start_span = self.advance().span; // consume 'if'
        let condition = if let Some(expr) = self.try_parse_expression() {
            expr
        } else {
            let span = self.current_span();
            let found = self
                .peek()
                .map(|t| format!("{:?}", t.as_ref().unwrap().token_type))
                .unwrap_or_else(|| "EOF".to_string());
            self.errors.push(ParseError::UnexpectedToken {
                expected: "condition expression".to_string(),
                found,
                span,
            });
            return None;
        };

        if !self.expect(&TokenType::Then, "'then'") {
            return None;
        }

        let then_block = self.parse_block();

        let mut elseif_clauses = Vec::new();
        while self.check(&TokenType::ElseIf) {
            let elseif_start_span = self.advance().span; // consume 'elseif'
            let elseif_condition = if let Some(expr) = self.try_parse_expression() {
                expr
            } else {
                let span = self.current_span();
                let found = self
                    .peek()
                    .map(|t| format!("{:?}", t.as_ref().unwrap().token_type))
                    .unwrap_or_else(|| "EOF".to_string());
                self.errors.push(ParseError::UnexpectedToken {
                    expected: "condition expression".to_string(),
                    found,
                    span,
                });
                return None;
            };

            if !self.expect(&TokenType::Then, "'then'") {
                return None;
            }

            let elseif_block = self.parse_block();
            let elseif_span = Span::new(elseif_start_span.start, self.previous_span.end);

            elseif_clauses.push(ElseIfClause {
                condition: self.builder.alloc(elseif_condition),
                block: elseif_block,
                span: elseif_span,
            });
        }

        let else_block = if self.check(&TokenType::Else) {
            self.advance(); // consume 'else'
            Some(self.parse_block())
        } else {
            None
        };

        if !self.expect(&TokenType::End, "'end'") {
            return None;
        }
        let end_span = self.previous_span; // consumed 'end'

        let span = Span::new(start_span.start, end_span.end);

        Some(Statement::If {
            condition: self.builder.alloc(condition),
            then_block,
            elseif_clauses: self.builder.alloc_slice(&elseif_clauses),
            else_block,
            span,
        })
    }

    /// Parse an assignment statement
    fn parse_assignment_statement(
        &mut self,
        variables: Vec<&'arena Variable<'arena>>,
    ) -> Option<Statement<'arena>> {
        if !self.check(&TokenType::Assign) {
            let span = self.current_span();
            self.errors.push(ParseError::InvalidSyntax {
                message: "Expected '=' for assignment".to_string(),
                span,
            });
            return None;
        }
        self.advance(); // consume '='

        let expressions = self.parse_required_expression_list();

        let start_span = variables
            .first()
            .map(|v| v.span())
            .unwrap_or_else(|| self.current_span());
        let end_span = self.previous_span;
        let span = Span::new(start_span.start, end_span.end);

        Some(Statement::Assignment {
            variables: self.builder.alloc_slice(&variables),
            expressions,
            span,
        })
    }

    /// Try to parse a prefix expression, which can be a variable or a function call
    fn try_parse_prefix_expression(&mut self) -> Option<Expression<'arena>> {
        self.skip_trivia(); // Skip trivia before parsing
        let token_res = self.peek().map(|r| r.as_ref());

        let mut expr = if let Some(Ok(token)) = token_res {
            match &token.token_type {
                TokenType::Identifier(_) | TokenType::LeftParen => {
                    let token = self.advance();
                    let span = token.span;
                    match token.token_type {
                        TokenType::Identifier(name) => {
                            let var = self.builder.alloc(Variable::Identifier {
                                name: self.builder.alloc_str(name),
                                span,
                            });
                            Expression::Variable { var, span }
                        }
                        TokenType::LeftParen => {
                            let inner_expr = if let Some(expr) = self.try_parse_expression() {
                                expr
                            } else {
                                let span = self.current_span();
                                let found = self
                                    .peek()
                                    .map(|t| format!("{:?}", t.as_ref().unwrap().token_type))
                                    .unwrap_or_else(|| "EOF".to_string());
                                self.errors.push(ParseError::UnexpectedToken {
                                    expected: "expression".to_string(),
                                    found,
                                    span,
                                });
                                return None;
                            };
                            if !self.check(&TokenType::RightParen) {
                                let span = self.current_span();
                                let found = self
                                    .peek()
                                    .map(|t| format!("{:?}", t.as_ref().unwrap().token_type))
                                    .unwrap_or_else(|| "EOF".to_string());
                                self.errors.push(ParseError::UnexpectedToken {
                                    expected: "')'".to_string(),
                                    found,
                                    span,
                                });
                                return None;
                            }
                            let end_span = self.advance().span;
                            Expression::Parenthesized {
                                expression: self.builder.alloc(inner_expr),
                                span: Span::new(span.start, end_span.end),
                            }
                        }
                        _ => {
                            self.errors.push(ParseError::InvalidSyntax {
                                message: format!(
                                    "Unexpected token in prefix expression: {:?}",
                                    token.token_type
                                ),
                                span: token.span,
                            });
                            return None;
                        }
                    }
                }
                _ => {
                    if let Some(simple_expr) = self.try_parse_simple_expression() {
                        simple_expr
                    } else {
                        return None;
                    }
                }
            }
        } else {
            if let Some(Err(e)) = self.peeked.as_ref() {
                self.errors.push(e.clone());
            }
            return None;
        };

        // Handle field access, indexing, and function calls
        loop {
            let peeked_token_type = self.peek().map(|r| r.as_ref().map(|t| &t.token_type));
            match peeked_token_type {
                Some(Ok(token_type)) => {
                    match token_type {
                        TokenType::LeftBracket => {
                            self.advance(); // consume '['
                            let index_expr = if let Some(expr) = self.try_parse_expression() {
                                expr
                            } else {
                                let span = self.current_span();
                                let found = self
                                    .peek()
                                    .map(|t| format!("{:?}", t.as_ref().unwrap().token_type))
                                    .unwrap_or_else(|| "EOF".to_string());
                                self.errors.push(ParseError::UnexpectedToken {
                                    expected: "index expression".to_string(),
                                    found,
                                    span,
                                });
                                return None;
                            };
                            if !self.check(&TokenType::RightBracket) {
                                let span = self.current_span();
                                self.errors.push(ParseError::InvalidSyntax {
                                    message: "Expected ']' after index".to_string(),
                                    span,
                                });
                                return None;
                            }
                            let end_token = self.advance(); // consume ']'
                            let span = Span::new(expr.span().start, end_token.span.end);
                            let table = self.builder.alloc(expr);
                            let index = self.builder.alloc(index_expr);
                            let var = self.builder.alloc(Variable::Index { table, index, span });
                            expr = Expression::Variable { var, span };
                        }
                        TokenType::Dot => {
                            self.advance(); // consume '.'
                            if let Some(Ok(Token {
                                token_type: TokenType::Identifier(_),
                                ..
                            })) = self.peek()
                            {
                                let field_token = self.advance();
                                let field = if let TokenType::Identifier(f) = field_token.token_type
                                {
                                    f
                                } else {
                                    ""
                                }; // Should be unreachable
                                let span = Span::new(expr.span().start, field_token.span.end);
                                let table = self.builder.alloc(expr);
                                let var = self.builder.alloc(Variable::Field {
                                    table,
                                    field: self.builder.alloc_str(field),
                                    span,
                                });
                                expr = Expression::Variable { var, span };
                            } else {
                                let span = self.current_span();
                                self.errors.push(ParseError::InvalidSyntax {
                                    message: "Expected identifier after '.'".to_string(),
                                    span,
                                });
                                return None;
                            }
                        }
                        // Function call args
                        TokenType::LeftParen | TokenType::LeftBrace | TokenType::String(_) => {
                            let args = self.parse_args();
                            let end_span = self.previous_span;
                            let span = Span::new(expr.span().start, end_span.end);
                            let func = self.builder.alloc(expr);
                            expr = Expression::FunctionCall {
                                function: func,
                                method: None,
                                arguments: args,
                                span,
                            };
                        }
                        TokenType::Colon => {
                            self.advance(); // consume ':'
                            if let Some(Ok(Token {
                                token_type: TokenType::Identifier(_),
                                ..
                            })) = self.peek()
                            {
                                let method_token = self.advance();
                                let method =
                                    if let TokenType::Identifier(m) = method_token.token_type {
                                        m
                                    } else {
                                        ""
                                    }; // Should be unreachable
                                let args = self.parse_args();
                                let end_span = self.previous_span;
                                let span = Span::new(expr.span().start, end_span.end);
                                let func = self.builder.alloc(expr);
                                expr = Expression::FunctionCall {
                                    function: func,
                                    method: Some(self.builder.alloc_str(method)),
                                    arguments: args,
                                    span,
                                };
                            } else {
                                let span = self.current_span();
                                self.errors.push(ParseError::InvalidSyntax {
                                    message: "Expected identifier after ':'".to_string(),
                                    span,
                                });
                                return None;
                            }
                        }
                        _ => break,
                    }
                }
                _ => break,
            }
        }

        Some(expr)
    }

    /// Parses simple expressions like literals, table constructors, and function definitions.
    fn try_parse_simple_expression(&mut self) -> Option<Expression<'arena>> {
        if let Some(Ok(token)) = self.peek() {
            let token_type = &token.token_type; // Reference to TokenType
            let span = token.span; // Copy Span
            match token_type {
                TokenType::Ellipsis => {
                    self.advance();
                    Some(Expression::Varargs { span })
                }
                TokenType::String(value) => {
                    let value_str = *value; // Extract the string slice before advance
                    self.advance();
                    Some(Expression::String {
                        value: self.builder.alloc_str(value_str),
                        span,
                    })
                }
                TokenType::Number(value) => {
                    let value_str = *value; // Extract the string slice before advance
                    self.advance();
                    Some(Expression::Number {
                        value: self.builder.alloc_str(value_str),
                        span,
                    })
                }
                TokenType::Nil => {
                    self.advance();
                    Some(Expression::Nil { span })
                }
                TokenType::True => {
                    self.advance();
                    Some(Expression::Boolean { value: true, span })
                }
                TokenType::False => {
                    self.advance();
                    Some(Expression::Boolean { value: false, span })
                }
                TokenType::LeftBrace => self.parse_table_constructor(),
                TokenType::Function => {
                    self.advance();
                    // Anonymous function literal: function funcbody
                    let body = self.parse_function_body();
                    let span = Span::new(span.start, body.span.end);
                    Some(Expression::Function {
                        body: self.builder.alloc(body),
                        span,
                    })
                }
                _ => None,
            }
        } else {
            None
        }
    }

    /// Parse a table constructor expression
    fn parse_table_constructor(&mut self) -> Option<Expression<'arena>> {
        let start_span = self.advance().span; // consume '{'
        let mut fields = Vec::new();
        if !self.check(&TokenType::RightBrace) {
            loop {
                if self.check(&TokenType::RightBrace) {
                    break;
                }
                if let Some(field) = self.parse_table_field() {
                    fields.push(field);
                } else {
                    self.synchronize();
                }

                if self.check(&TokenType::Comma) || self.check(&TokenType::Semicolon) {
                    self.advance(); // consume separator
                    if self.check(&TokenType::RightBrace) {
                        break;
                    }
                } else {
                    break;
                }
            }
        }

        if !self.expect(&TokenType::RightBrace, "'}' to close table constructor") {
            return None;
        }
        let end_span = self.previous_span; // already consumed '}'
        let span = Span::new(start_span.start, end_span.end);

        Some(Expression::Table {
            fields: self.builder.alloc_slice(&fields),
            span,
        })
    }

    /// Parse a single field in a table constructor
    fn parse_table_field(&mut self) -> Option<TableField<'arena>> {
        // Try parsing [exp] = exp
        if self.check(&TokenType::LeftBracket) {
            self.advance(); // consume '['
            let key = if let Some(expr) = self.try_parse_expression() {
                expr
            } else {
                let span = self.current_span();
                let found = self
                    .peek()
                    .map(|t| format!("{:?}", t.as_ref().unwrap().token_type))
                    .unwrap_or_else(|| "EOF".to_string());
                self.errors.push(ParseError::UnexpectedToken {
                    expected: "key expression".to_string(),
                    found,
                    span,
                });
                return None;
            };

            if !self.check(&TokenType::RightBracket) {
                let span = self.current_span();
                self.errors.push(ParseError::InvalidSyntax {
                    message: "Expected ']' after table key".to_string(),
                    span,
                });
                return None;
            }
            self.advance(); // consume ']'

            if self.check(&TokenType::Assign) {
                self.advance(); // consume '='
                let value = if let Some(expr) = self.try_parse_expression() {
                    expr
                } else {
                    let span = self.current_span();
                    let found = self
                        .peek()
                        .map(|t| format!("{:?}", t.as_ref().unwrap().token_type))
                        .unwrap_or_else(|| "EOF".to_string());
                    self.errors.push(ParseError::UnexpectedToken {
                        expected: "value expression".to_string(),
                        found,
                        span,
                    });
                    return None;
                };

                let span = Span::new(key.span().start, value.span().end);
                return Some(TableField::Index {
                    key: self.builder.alloc(key),
                    value: self.builder.alloc(value),
                    span,
                });
            } else {
                // In Lua, a table field of the form `[expression]` must be followed by `=`.
                let span = self.current_span();
                self.errors.push(ParseError::InvalidSyntax {
                    message: "Expected '=' after table index expression".to_string(),
                    span,
                });
                return None;
            }
        }

        // At this point, it's either `name = exp` or just `exp`.
        // We can parse an expression and then check if the next token is `=`.
        let expr = if let Some(expr) = self.try_parse_expression() {
            expr
        } else {
            // No expression found where a field was expected.
            // This can happen with trailing commas, e.g., `{ a, }`
            // The loop in `parse_table_constructor` should handle this.
            return None;
        };

        if self.check(&TokenType::Assign) {
            // This looks like a `key = value` field.
            // The key must have been a simple identifier.
            if let Expression::Variable {
                var: Variable::Identifier { name, .. },
                span: key_span,
            } = expr
            {
                self.advance(); // consume '='
                let value = if let Some(value_expr) = self.try_parse_expression() {
                    value_expr
                } else {
                    let span = self.current_span();
                    let found = self
                        .peek()
                        .map(|t| format!("{:?}", t.as_ref().unwrap().token_type))
                        .unwrap_or_else(|| "EOF".to_string());
                    self.errors.push(ParseError::UnexpectedToken {
                        expected: "value expression".to_string(),
                        found,
                        span,
                    });
                    return None;
                };
                let span = Span::new(key_span.start, value.span().end);
                return Some(TableField::Named {
                    key: name,
                    value: self.builder.alloc(value),
                    span,
                });
            } else {
                // The LHS of the `=` was not an identifier, which is a syntax error.
                self.errors.push(ParseError::InvalidSyntax {
                    message: "Invalid key in table field assignment; expected an identifier"
                        .to_string(),
                    span: expr.span(),
                });
                // Recovery: we'll treat the parsed expression as a list item
                // and let the parser complain about the unexpected '=' later.
                let span = expr.span();
                return Some(TableField::List {
                    value: self.builder.alloc(expr),
                    span,
                });
            }
        }

        // It's a list-style field
        let span = expr.span();
        Some(TableField::List {
            value: self.builder.alloc(expr),
            span,
        })
    }

    fn parse_args(&mut self) -> &'arena [Expression<'arena>] {
        let peeked_token_type = self.peek().map(|r| r.as_ref().map(|t| &t.token_type));
        match peeked_token_type {
            Some(Ok(TokenType::LeftParen)) => {
                self.advance(); // consume '('
                let list = if !self.check(&TokenType::RightParen) {
                    self.parse_expression_list()
                } else {
                    &[]
                };
                if self.check(&TokenType::RightParen) {
                    self.advance();
                }
                list
            }
            Some(Ok(TokenType::LeftBrace)) => {
                if let Some(expr) = self.parse_table_constructor() {
                    self.builder.alloc_slice(&[expr])
                } else {
                    &[]
                }
            }
            Some(Ok(TokenType::String(_))) => {
                if let Some(expr) = self.try_parse_expression() {
                    self.builder.alloc_slice(&[expr])
                } else {
                    &[]
                }
            }
            _ => &[],
        }
    }

    /// Parse a return statement
    fn parse_return_statement(&mut self) -> Option<&'arena ReturnStatement<'arena>> {
        let start_span = self.advance().span; // consume 'return'

        // Parse optional expression list
        let expressions = if !self.is_at_end()
            && !matches!(
                self.peek()
                    .and_then(|t| t.as_ref().ok())
                    .map(|t| &t.token_type),
                Some(TokenType::Semicolon) | Some(TokenType::Eof) | None
            ) {
            let expr_list = self.parse_expression_list();
            Some(expr_list)
        } else {
            None
        };

        // Optional semicolon
        let end_span = if self.check(&TokenType::Semicolon) {
            self.advance().span
        } else {
            self.previous_span
        };

        let span = Span::new(start_span.start, end_span.end);

        Some(self.builder.alloc(ReturnStatement { expressions, span }))
    }

    /// Parse expression list (comma-separated expressions)
    fn parse_expression_list(&mut self) -> &'arena [Expression<'arena>] {
        let mut expressions = Vec::new();

        if let Some(first_expr) = self.try_parse_expression() {
            expressions.push(first_expr);

            while self.check(&TokenType::Comma) {
                self.advance(); // consume comma
                if let Some(expr) = self.try_parse_expression() {
                    expressions.push(expr);
                } else {
                    break;
                }
            }
        }

        self.builder.alloc_slice(&expressions)
    }

    /// Parse expression list that requires at least one expression
    fn parse_required_expression_list(&mut self) -> &'arena [Expression<'arena>] {
        let first_expr = if let Some(expr) = self.try_parse_expression() {
            expr
        } else {
            let span = self.current_span();
            let found = self
                .peek()
                .map(|t| format!("{:?}", t.as_ref().unwrap().token_type))
                .unwrap_or_else(|| "EOF".to_string());
            self.errors.push(ParseError::UnexpectedToken {
                expected: "expression".to_string(),
                found,
                span,
            });
            return &[];
        };

        let mut expressions = vec![first_expr];

        while self.check(&TokenType::Comma) {
            self.advance(); // consume comma
            if let Some(expr) = self.try_parse_expression() {
                expressions.push(expr);
            } else {
                break;
            }
        }

        self.builder.alloc_slice(&expressions)
    }

    /// Try to parse an expression using precedence climbing
    fn try_parse_expression(&mut self) -> Option<Expression<'arena>> {
        self.try_parse_expression_with_precedence(0)
    }

    /// Parse expression with minimum precedence (Pratt/precedence climbing parser)
    fn try_parse_expression_with_precedence(
        &mut self,
        min_precedence: u8,
    ) -> Option<Expression<'arena>> {
        // Parse the left-hand side (could be a prefix expression or unary operator)
        let mut left = if let Some(unary_op) = self.peek_unary_op() {
            // Handle unary operators
            let start_span = self.advance().span; // consume unary operator

            // Recursively parse the operand with unary operator precedence
            let operand = if let Some(expr) =
                self.try_parse_expression_with_precedence(unary_op.precedence())
            {
                expr
            } else {
                let span = self.current_span();
                self.errors.push(ParseError::InvalidSyntax {
                    message: format!("Expected expression after unary operator {:?}", unary_op),
                    span,
                });
                return None;
            };

            let span = Span::new(start_span.start, operand.span().end);
            Expression::UnaryOp {
                operator: unary_op,
                operand: self.builder.alloc(operand),
                span,
            }
        } else if let Some(expr) = self.try_parse_prefix_expression() {
            expr
        } else {
            return None;
        };

        // Parse binary operators with precedence climbing
        while let Some(op) = self.peek_binary_op() {
            if op.precedence() < min_precedence {
                break;
            }

            self.advance(); // consume the operator

            // For right-associative operators, use the same precedence
            // For left-associative operators, use precedence + 1
            let next_min_precedence = if op.is_right_associative() {
                op.precedence()
            } else {
                op.precedence() + 1
            };

            // Recursively parse the right-hand side
            let right = if let Some(expr) =
                self.try_parse_expression_with_precedence(next_min_precedence)
            {
                expr
            } else {
                let span = self.current_span();
                self.errors.push(ParseError::InvalidSyntax {
                    message: format!("Expected expression after binary operator {:?}", op),
                    span,
                });
                return None;
            };

            // Combine left and right into a binary operation
            let span = Span::new(left.span().start, right.span().end);
            left = Expression::BinaryOp {
                left: self.builder.alloc(left),
                operator: op,
                right: self.builder.alloc(right),
                span,
            };
        }

        Some(left)
    }

    fn peek_binary_op(&mut self) -> Option<BinaryOp> {
        self.skip_trivia(); // Skip trivia before checking for binary operators
        self.peek().and_then(|res| {
            res.as_ref()
                .ok()
                .and_then(|t| BinaryOp::from_token(&t.token_type))
        })
    }

    fn peek_unary_op(&mut self) -> Option<UnaryOp> {
        self.skip_trivia(); // Skip trivia before checking for unary operators
        self.peek().and_then(|res| {
            res.as_ref()
                .ok()
                .and_then(|t| UnaryOp::from_token(&t.token_type))
        })
    }

    // === Token management utilities ===

    /// Check if current token matches the given type
    fn check(&mut self, token_type: &TokenType) -> bool {
        self.skip_trivia();
        if let Some(current_token) = self.peek() {
            match current_token {
                Ok(token) => {
                    std::mem::discriminant(&token.token_type) == std::mem::discriminant(token_type)
                }
                Err(_) => false,
            }
        } else {
            false
        }
    }

    /// Peek at current token without consuming
    fn peek(&mut self) -> Option<&Result<Token<'input>, ParseError>> {
        if self.peeked.is_none() {
            self.peeked = self.lexer.next().map(|res| res.map_err(|e| e.into()));
        }
        self.peeked.as_ref()
    }

    /// Advance to next token and return the consumed token
    fn advance(&mut self) -> Token<'input> {
        let next = self
            .peeked
            .take()
            .or_else(|| self.lexer.next().map(|res| res.map_err(|e| e.into())));
        match next {
            Some(Ok(token)) => {
                self.previous_span = token.span;
                token
            }
            Some(Err(e)) => {
                self.errors.push(e);
                let last_pos = self.lexer.current_position();
                self.lexer.make_token(TokenType::Eof, last_pos, last_pos)
            }
            None => {
                let last_pos = self.lexer.current_position();
                self.lexer.make_token(TokenType::Eof, last_pos, last_pos)
            }
        }
    }

    /// Check if we're at the end of tokens
    fn is_at_end(&mut self) -> bool {
        self.skip_trivia();
        self.peek().is_none()
    }

    /// Get current token span
    fn current_span(&mut self) -> Span {
        self.skip_trivia();
        let default_pos = self.lexer.current_position();
        self.peek()
            .map(|res| match res {
                Ok(t) => t.span,
                Err(e) => e.span().unwrap_or_else(|| Span::single(default_pos)),
            })
            .unwrap_or_else(|| Span::single(default_pos))
    }

    /// Check if current token is a block terminator
    fn is_block_terminator(&mut self) -> bool {
        self.skip_trivia();
        let current_token = self
            .peek()
            .and_then(|res| res.as_ref().ok())
            .map(|t| &t.token_type);
        matches!(
            current_token,
            Some(TokenType::End)
                | Some(TokenType::Else)
                | Some(TokenType::ElseIf)
                | Some(TokenType::Until)
                | Some(TokenType::Return)
                | Some(TokenType::Eof)
        ) || current_token.is_none()
    }

    fn synchronize(&mut self) {
        self.advance(); // Consume the token that caused the error

        while !self.is_at_end() {
            // Check for tokens that can start a new statement
            match self.peek().map(|r| r.as_ref().ok().map(|t| &t.token_type)) {
                Some(Some(
                    TokenType::Semicolon
                    | TokenType::Local
                    | TokenType::Function
                    | TokenType::If
                    | TokenType::While
                    | TokenType::For
                    | TokenType::Repeat
                    | TokenType::Do
                    | TokenType::Break
                    | TokenType::Goto
                    | TokenType::DoubleColon
                    | TokenType::Return,
                )) => break,
                _ => {
                    self.advance();
                } // Consume tokens until we find a recovery point
            };
        }
    }
}

// === Parser combinator macros for clean parsing ===
// Note: These macros are defined for future use but not currently utilized
