package ideah.tree.decl;

import ideah.tree.Ident;
import ideah.tree.Located;
import ideah.util.LineColRange;

public class ConDecl extends Located {

    public final Ident name;
    // todo


    public ConDecl(LineColRange location, Ident name) {
        super(location);
        this.name = name;
    }
}
