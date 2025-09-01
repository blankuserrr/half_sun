use crate::ast::*;

pub trait Visitor<'arena> {
    fn visit_statement(&mut self, stmt: &Statement<'arena>) {
        walk_statement(self, stmt);
    }

    fn visit_expression(&mut self, expr: &Expression<'arena>) {
        walk_expression(self, expr);
    }

    fn visit_block(&mut self, block: &Block<'arena>) {
        walk_block(self, block);
    }
}

pub fn walk_block<'arena, V: Visitor<'arena> + ?Sized>(visitor: &mut V, block: &Block<'arena>) {
    for stmt in block.statements {
        visitor.visit_statement(stmt);
    }
    if let Some(ret) = &block.return_stmt {
        for expr in ret.expressions.iter().flat_map(|e| e.iter()) {
            visitor.visit_expression(expr);
        }
    }
}

pub fn walk_statement<'arena, V: Visitor<'arena> + ?Sized>(
    visitor: &mut V,
    stmt: &Statement<'arena>,
) {
    match stmt {
        Statement::If {
            condition,
            then_block,
            elseif_clauses,
            else_block,
            ..
        } => {
            visitor.visit_expression(condition);
            visitor.visit_block(then_block);
            for clause in *elseif_clauses {
                visitor.visit_expression(clause.condition);
                visitor.visit_block(clause.block);
            }
            if let Some(else_block) = else_block {
                visitor.visit_block(else_block);
            }
        }
        Statement::Assignment {
            variables,
            expressions,
            ..
        } => {
            for var in *variables {
                walk_variable(visitor, var);
            }
            for expr in *expressions {
                visitor.visit_expression(expr);
            }
        }
        Statement::Expression { expression, .. } => {
            visitor.visit_expression(expression);
        }
        Statement::Do { block, .. } => {
            visitor.visit_block(block);
        }
        Statement::While {
            condition, body, ..
        } => {
            visitor.visit_expression(condition);
            visitor.visit_block(body);
        }
        Statement::Repeat {
            body, condition, ..
        } => {
            visitor.visit_block(body);
            visitor.visit_expression(condition);
        }
        Statement::NumericFor {
            start,
            end,
            step,
            body,
            ..
        } => {
            visitor.visit_expression(start);
            visitor.visit_expression(end);
            if let Some(step) = step {
                visitor.visit_expression(step);
            }
            visitor.visit_block(body);
        }
        Statement::GenericFor {
            expressions, body, ..
        } => {
            for expr in *expressions {
                visitor.visit_expression(expr);
            }
            visitor.visit_block(body);
        }
        Statement::FunctionDef { body, .. } => {
            walk_function_body(visitor, body);
        }
        Statement::LocalFunction { body, .. } => {
            walk_function_body(visitor, body);
        }
        Statement::LocalVariables { expressions, .. } => {
            if let Some(expressions) = expressions {
                for expr in *expressions {
                    visitor.visit_expression(expr);
                }
            }
        }
        _ => {}
    }
}

pub fn walk_expression<'arena, V: Visitor<'arena> + ?Sized>(
    visitor: &mut V,
    expr: &Expression<'arena>,
) {
    match expr {
        Expression::Variable { var, .. } => {
            walk_variable(visitor, var);
        }
        Expression::FunctionCall {
            function,
            arguments,
            ..
        } => {
            visitor.visit_expression(function);
            for arg in *arguments {
                visitor.visit_expression(arg);
            }
        }
        Expression::BinaryOp { left, right, .. } => {
            visitor.visit_expression(left);
            visitor.visit_expression(right);
        }
        Expression::UnaryOp { operand, .. } => {
            visitor.visit_expression(operand);
        }
        Expression::Table { fields, .. } => {
            for field in *fields {
                match field {
                    TableField::Index { key, value, .. } => {
                        visitor.visit_expression(key);
                        visitor.visit_expression(value);
                    }
                    TableField::Named { value, .. } => {
                        visitor.visit_expression(value);
                    }
                    TableField::List { value, .. } => {
                        visitor.visit_expression(value);
                    }
                }
            }
        }
        Expression::Function { body, .. } => {
            walk_function_body(visitor, body);
        }
        Expression::Parenthesized { expression, .. } => {
            visitor.visit_expression(expression);
        }
        _ => {}
    }
}

pub fn walk_variable<'arena, V: Visitor<'arena> + ?Sized>(visitor: &mut V, var: &Variable<'arena>) {
    match var {
        Variable::Index { table, index, .. } => {
            visitor.visit_expression(table);
            visitor.visit_expression(index);
        }
        Variable::Field { table, .. } => {
            visitor.visit_expression(table);
        }
        _ => {}
    }
}

pub fn walk_function_body<'arena, V: Visitor<'arena> + ?Sized>(
    visitor: &mut V,
    body: &FunctionBody<'arena>,
) {
    visitor.visit_block(body.body);
}
