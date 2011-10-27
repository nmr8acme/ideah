package ideah.tree.expr;

import ideah.tree.Match;
import ideah.util.IRange;

import java.util.List;

public final class Lambda extends Expression {

    public final List<Match> matches;

    public Lambda(IRange location, List<Match> matches) {
        super(location);
        this.matches = matches;
    }

    protected Iterable<Match> getChildren() {
        return matches;
    }
}
