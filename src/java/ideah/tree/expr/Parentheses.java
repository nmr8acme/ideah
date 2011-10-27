package ideah.tree.expr;

import ideah.util.IRange;

import java.util.Arrays;

public final class Parentheses extends Expression {

    public final Expression expression;

    public Parentheses(IRange location, Expression expression) {
        super(location);
        this.expression = expression;
    }

    protected Iterable<Expression> getChildren() {
        return Arrays.asList(expression);
    }
}
