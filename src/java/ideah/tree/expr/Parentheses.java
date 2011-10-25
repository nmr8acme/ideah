package ideah.tree.expr;

import ideah.util.LineColRange;

public final class Parentheses extends Expression {

    public final Expression expression;

    public Parentheses(LineColRange location, Expression expression) {
        super(location);
        this.expression = expression;
    }
}
