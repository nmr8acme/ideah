package ideah.tree.expr;

import ideah.tree.IRange;

import java.util.Arrays;

public final class LeftSection extends Expression {

    public final Expression arg;
    public final Expression op;

    public LeftSection(IRange location, Expression arg, Expression op) {
        super(location);
        this.arg = arg;
        this.op = op;
    }

    protected Iterable<Expression> getChildren() {
        return Arrays.asList(arg, op);
    }
}
