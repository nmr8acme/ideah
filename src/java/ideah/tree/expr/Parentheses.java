package ideah.tree.expr;

public final class Parentheses extends Expression {

    public final Expression expression;

    public Parentheses(Expression expression) {
        this.expression = expression;
    }
}
