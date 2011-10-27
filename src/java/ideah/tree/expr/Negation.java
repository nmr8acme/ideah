package ideah.tree.expr;

import ideah.util.LineColRange;

import java.util.Arrays;

public final class Negation extends Expression {

    public final Expression expression;

    public Negation(LineColRange location, Expression expression) {
        super(location);
        this.expression = expression;
    }

    protected Iterable<Expression> getChildren() {
        return Arrays.asList(expression);
    }
}
