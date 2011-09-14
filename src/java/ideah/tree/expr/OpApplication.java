package ideah.tree.expr;

public final class OpApplication extends Expression {

    public final Expression left;
    public final Expression op;
    public final Expression right;

    public OpApplication(Expression left, Expression op, Expression right) {
        this.left = left;
        this.op = op;
        this.right = right;
    }
}
