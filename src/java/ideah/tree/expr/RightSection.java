package ideah.tree.expr;

public final class RightSection extends Expression {

    public final Expression op;
    public final Expression arg;

    public RightSection(Expression op, Expression arg) {
        this.op = op;
        this.arg = arg;
    }
}
