package ideah.tree.expr;

import ideah.util.LineColRange;

import java.util.List;

public final class TupleExpr extends Expression {

    public final List<Expression> expressions;

    public TupleExpr(LineColRange location, List<Expression> expressions) {
        super(location);
        this.expressions = expressions;
    }

    protected Iterable<Expression> getChildren() {
        return expressions;
    }
}
