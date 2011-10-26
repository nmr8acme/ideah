package ideah.tree.expr;

import ideah.util.LineColRange;

import java.util.List;

public final class ListExpr extends Expression {

    public final List<Expression> expressions;

    public ListExpr(LineColRange location, List<Expression> expressions) {
        super(location);
        this.expressions = expressions;
    }
}
