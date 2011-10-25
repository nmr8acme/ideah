package ideah.tree.decl;

import ideah.tree.Ident;
import ideah.util.LineColRange;

import java.util.List;

public final class TypeDecl extends Declaration {

    public final Ident name;
    public final List<Ident> params;
    public final List<ConDecl> constructors;
    // todo: other

    public TypeDecl(LineColRange location, Ident name, List<Ident> params, List<ConDecl> constructors) {
        super(location);
        this.name = name;
        this.params = params;
        this.constructors = constructors;
    }
}
