package ideah.tree.expr;

import ideah.tree.Match;

import java.util.List;

public final class CaseExpr extends Expression {

    public final Expression expression;
    public final List<Match> matches;

    public CaseExpr(Expression expression, List<Match> matches) {
        this.expression = expression;
        this.matches = matches;
    }
}
