package ideah.tree.decl;

import ideah.tree.Ident;
import ideah.tree.Located;

public class ConDecl extends Located {

    public final Ident name;
    // todo


    public ConDecl(Ident name) {
        this.name = name;
    }
}
