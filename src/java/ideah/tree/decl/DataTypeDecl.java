package ideah.tree.decl;

import com.google.common.collect.Iterables;
import ideah.tree.Ident;
import ideah.tree.Located;
import ideah.util.IRange;

import java.util.Arrays;
import java.util.List;

public final class DataTypeDecl extends TyClDecl {

    public final Ident name;
    public final List<ConDecl> constructors;
    // todo: other

    public DataTypeDecl(IRange location, Ident name, List<ConDecl> constructors) {
        super(location);
        this.name = name;
        this.constructors = constructors;
    }

    protected Iterable<Located> getChildren() {
        return Iterables.concat(Arrays.asList(name), constructors);
    }
}
