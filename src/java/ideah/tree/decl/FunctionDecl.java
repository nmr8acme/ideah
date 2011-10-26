package ideah.tree.decl;

import ideah.tree.Ident;
import ideah.tree.Match;
import ideah.util.LineColRange;

import java.util.List;

public final class FunctionDecl extends Bind {

    public final Ident name;
    public final List<Match> matches;

    public FunctionDecl(LineColRange location, Ident name, List<Match> matches) {
        super(location);
        this.name = name;
        this.matches = matches;
    }
}
