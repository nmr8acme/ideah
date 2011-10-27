package ideah.tree.decl;

import ideah.tree.Ident;
import ideah.tree.Located;
import ideah.tree.type.Type;
import ideah.util.LineColRange;

import java.util.Arrays;

public final class TypeSigDecl extends SigDecl {

    public final Ident name;
    public final Type type;

    public TypeSigDecl(LineColRange location, Ident name, Type type) {
        super(location);
        this.name = name;
        this.type = type;
    }

    protected Iterable<Located> getChildren() {
        return Arrays.asList(name, type);
    }
}
