package ideah.tree.expr;

import ideah.util.LineColRange;

import java.util.Arrays;

public final class Parentheses extends Expression {

    public final Expression expression;

    public Parentheses(LineColRange location, Expression expression) {
        super(location);
        this.expression = expression;
    }

    protected Iterable<Expression> getChildren() {
        return Arrays.asList(expression);
    }
}
