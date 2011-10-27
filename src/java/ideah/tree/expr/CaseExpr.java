package ideah.tree.expr;

import com.google.common.collect.Iterables;
import ideah.tree.Located;
import ideah.tree.Match;
import ideah.util.IRange;

import java.util.Arrays;
import java.util.List;

public final class CaseExpr extends Expression {

    public final Expression expression;
    public final List<Match> matches;

    public CaseExpr(IRange location, Expression expression, List<Match> matches) {
        super(location);
        this.expression = expression;
        this.matches = matches;
    }

    protected Iterable<Located> getChildren() {
        return Iterables.concat(Arrays.asList(expression), matches);
    }
}
