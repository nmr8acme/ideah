package ideah.tree.expr;

import ideah.util.LineColRange;

import java.util.Arrays;

public final class RightSection extends Expression {

    public final Expression op;
    public final Expression arg;

    public RightSection(LineColRange location, Expression op, Expression arg) {
        super(location);
        this.op = op;
        this.arg = arg;
    }

    protected Iterable<Expression> getChildren() {
        return Arrays.asList(op, arg);
    }
}
