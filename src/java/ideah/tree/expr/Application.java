package ideah.tree.expr;

public final class Application extends Expression {

    public final Expression function;
    public final Expression arg;

    public Application(Expression function, Expression arg) {
        this.function = function;
        this.arg = arg;
    }
}
