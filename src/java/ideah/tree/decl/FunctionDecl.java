package ideah.tree.decl;

import ideah.tree.Ident;
import ideah.tree.Match;

import java.util.List;

public final class FunctionDecl extends Declaration {

    public final Ident name;
    public final List<Match> matches;

    public FunctionDecl(Ident name, List<Match> matches) {
        this.name = name;
        this.matches = matches;
    }
}
