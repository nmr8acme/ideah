package ideah.tree.expr;

public final class LeftSection extends Expression {

    public final Expression arg;
    public final Expression op;

    public LeftSection(Expression arg, Expression op) {
        this.arg = arg;
        this.op = op;
    }
}
