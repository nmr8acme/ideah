package ideah.tree.decl;

import ideah.tree.Ident;
import ideah.tree.type.Type;
import ideah.util.LineColRange;

public final class TypeSigDecl extends SigDecl {

    public final Ident name;
    public final Type type;

    public TypeSigDecl(LineColRange location, Ident name, Type type) {
        super(location);
        this.name = name;
        this.type = type;
    }
}
