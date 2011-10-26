package ideah.tree.expr;

import ideah.util.LineColRange;

public final class Negation extends Expression {

    public final Expression expression;

    public Negation(LineColRange location, Expression expression) {
        super(location);
        this.expression = expression;
    }
}
