package ideah.tree.expr;

import ideah.util.IRange;

import java.util.Arrays;

public final class OpApplication extends Expression {

    public final Expression left;
    public final Expression op;
    public final Expression right;

    public OpApplication(IRange location, Expression left, Expression op, Expression right) {
        super(location);
        this.left = left;
        this.op = op;
        this.right = right;
    }

    protected Iterable<Expression> getChildren() {
        return Arrays.asList(left, op, right);
    }
}
