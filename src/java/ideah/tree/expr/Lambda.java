package ideah.tree.expr;

import ideah.tree.Match;

import java.util.List;

public final class Lambda {

    public final List<Match> matches;

    public Lambda(List<Match> matches) {
        this.matches = matches;
    }
}
