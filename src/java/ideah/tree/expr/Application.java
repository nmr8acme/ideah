package ideah.tree.expr;

import ideah.util.LineColRange;

import java.util.Arrays;

public final class Application extends Expression {

    public final Expression function;
    public final Expression arg;

    public Application(LineColRange location, Expression function, Expression arg) {
        super(location);
        this.function = function;
        this.arg = arg;
    }

    protected Iterable<Expression> getChildren() {
        return Arrays.asList(function, arg);
    }
}
