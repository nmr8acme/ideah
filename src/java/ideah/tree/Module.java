package ideah.tree;

import ideah.tree.decl.Declaration;
import ideah.util.LineColRange;

import java.util.List;

public final class Module extends Located {

    public final Ident name;
    public final List<Export> exports;
    public final List<Import> imports;
    public final List<Declaration> declarations;

    public Module(LineColRange location, Ident name, List<Export> exports, List<Import> imports, List<Declaration> declarations) {
        super(location);
        this.name = name;
        this.exports = exports;
        this.imports = imports;
        this.declarations = declarations;
    }
}
