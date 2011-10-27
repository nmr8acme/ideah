package ideah.tree.decl;

import ideah.tree.Match;
import ideah.util.LineColRange;

import java.util.List;

public final class FunctionDecl extends Bind {

    public final List<Match> matches;

    public FunctionDecl(LineColRange location, List<Match> matches) {
        super(location);
        this.matches = matches;
    }

    protected Iterable<Match> getChildren() {
        return matches;
    }
}
