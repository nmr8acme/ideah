package ideah.tree.expr;

import ideah.tree.Match;
import ideah.util.LineColRange;

import java.util.List;

public final class CaseExpr extends Expression {

    public final Expression expression;
    public final List<Match> matches;

    public CaseExpr(LineColRange location, Expression expression, List<Match> matches) {
        super(location);
        this.expression = expression;
        this.matches = matches;
    }
}
