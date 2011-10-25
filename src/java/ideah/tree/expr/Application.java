package ideah.tree.expr;

import ideah.util.LineColRange;

public final class Application extends Expression {

    public final Expression function;
    public final Expression arg;

    public Application(LineColRange location, Expression function, Expression arg) {
        super(location);
        this.function = function;
        this.arg = arg;
    }
}
