package ideah.tree.expr;

import ideah.tree.IRange;

import java.util.List;

public final class ListExpr extends Expression {

    public final List<Expression> expressions;

    public ListExpr(IRange location, List<Expression> expressions) {
        super(location);
        this.expressions = expressions;
    }

    protected Iterable<Expression> getChildren() {
        return expressions;
    }
}
