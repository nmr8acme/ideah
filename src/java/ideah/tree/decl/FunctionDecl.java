package ideah.tree.decl;

import ideah.tree.Match;
import ideah.util.IRange;

import java.util.List;

public final class FunctionDecl extends Bind {

    public final List<Match> matches;

    public FunctionDecl(IRange location, List<Match> matches) {
        super(location);
        this.matches = matches;
    }

    protected Iterable<Match> getChildren() {
        return matches;
    }
}
