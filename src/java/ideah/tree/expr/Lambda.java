package ideah.tree.expr;

import ideah.tree.Match;
import ideah.util.LineColRange;

import java.util.List;

public final class Lambda extends Expression {

    public final List<Match> matches;

    public Lambda(LineColRange location, List<Match> matches) {
        super(location);
        this.matches = matches;
    }
}
