package ideah.tree.expr;

import ideah.tree.IRange;

import java.util.Arrays;

public final class Negation extends Expression {

    public final Expression expression;

    public Negation(IRange location, Expression expression) {
        super(location);
        this.expression = expression;
    }

    protected Iterable<Expression> getChildren() {
        return Arrays.asList(expression);
    }
}
