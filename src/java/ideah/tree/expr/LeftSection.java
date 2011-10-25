package ideah.tree.expr;

import ideah.util.LineColRange;

public final class LeftSection extends Expression {

    public final Expression arg;
    public final Expression op;

    public LeftSection(LineColRange location, Expression arg, Expression op) {
        super(location);
        this.arg = arg;
        this.op = op;
    }
}
