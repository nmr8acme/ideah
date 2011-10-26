package ideah.tree.decl;

import ideah.tree.Ident;
import ideah.util.LineColRange;

import java.util.List;

public final class DataTypeDecl extends TyClDecl {

    public final Ident name;
    public final List<ConDecl> constructors;
    // todo: other

    public DataTypeDecl(LineColRange location, Ident name, List<ConDecl> constructors) {
        super(location);
        this.name = name;
        this.constructors = constructors;
    }
}
