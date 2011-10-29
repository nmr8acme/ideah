package ideah.tree.decl;

import ideah.tree.IRange;
import ideah.tree.Ident;
import ideah.tree.Located;
import ideah.tree.type.Type;

import java.util.Arrays;

public final class TypeSigDecl extends SigDecl {

    public final Ident name;
    public final Type type;

    public TypeSigDecl(IRange location, Ident name, Type type) {
        super(location);
        this.name = name;
        this.type = type;
    }

    protected Iterable<Located> getChildren() {
        return Arrays.asList(name, type);
    }
}
