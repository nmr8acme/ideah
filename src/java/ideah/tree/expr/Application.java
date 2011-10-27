package ideah.tree.expr;

import ideah.util.IRange;

import java.util.Arrays;

public final class Application extends Expression {

    public final Expression function;
    public final Expression arg;

    public Application(IRange location, Expression function, Expression arg) {
        super(location);
        this.function = function;
        this.arg = arg;
    }

    protected Iterable<Expression> getChildren() {
        return Arrays.asList(function, arg);
    }
}
