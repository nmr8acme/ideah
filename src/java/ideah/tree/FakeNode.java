package ideah.tree;

import ideah.tree.decl.*;
import ideah.tree.expr.*;
import ideah.tree.pat.*;
import ideah.tree.stmt.BindStmt;
import ideah.tree.stmt.ExprStmt;
import ideah.tree.stmt.LetStmt;
import ideah.tree.stmt.Statement;
import ideah.tree.type.*;
import ideah.util.LineColRange;

import java.util.ArrayList;
import java.util.List;

public final class FakeNode extends Located {

    public final String name;
    public final List<FakeNode> children;

    public FakeNode(LineColRange location, String name, List<FakeNode> children) {
        super(location);
        this.name = name;
        this.children = children;
    }

    private void append(StringBuilder buf, String indent) {
        buf.append(indent + name + " " + location + " {\n");
        String newIndent = "    " + indent;
        for (FakeNode child : children) {
            child.append(buf, newIndent);
        }
        buf.append(indent + "}\n");
    }

    public String toString() {
        StringBuilder buf = new StringBuilder();
        append(buf, "");
        return buf.toString();
    }

    // todo: range for module!
    ModuleTree toModule(LineColRange location) throws NoMatchException {
        if ("Module".equals(name)) {
            int i = 0;
            Ident name = null;
            if (i < children.size()) {
                try {
                    name = children.get(i).toModuleName();
                    i++;
                } catch (NoMatchException ex) {
                    // ignore
                }
            }
            List<Export> exports = new ArrayList<Export>();
            while (i < children.size()) {
                FakeNode child = children.get(i);
                try {
                    Export export = child.toExport();
                    exports.add(export);
                } catch (NoMatchException ex) {
                    break;
                }
                i++;
            }
            List<Import> imports = new ArrayList<Import>();
            while (i < children.size()) {
                FakeNode child = children.get(i);
                try {
                    Import imp = child.toImport();
                    imports.add(imp);
                } catch (NoMatchException ex) {
                    break;
                }
                i++;
            }
            List<Declaration> declarations = new ArrayList<Declaration>();
            while (i < children.size()) {
                FakeNode child = children.get(i);
                Declaration declaration = child.toDeclaration();
                declarations.add(declaration);
                i++;
            }
            return new ModuleTree(location, name, exports, imports, declarations);
        } else {
            throw new NoMatchException();
        }
    }

    private Ident toModuleName() throws NoMatchException {
        if ("ModuleName".equals(name)) {
            return new Ident(location);
        } else {
            throw new NoMatchException();
        }
    }

    private Export toExport() throws NoMatchException {
        if ("Export".equals(name)) {
            return new Export(location);
        } else {
            throw new NoMatchException();
        }
    }

    private Import toImport() throws NoMatchException {
        if ("Import".equals(name)) {
            return new Import(location);
        } else {
            throw new NoMatchException();
        }
    }

    private Declaration toDeclaration() throws NoMatchException {
        if ("ValD".equals(name)) {
            return children.get(0).toBind();
        } else if ("TyClD".equals(name)) {
            return children.get(0).toTyClDeclaration();
        } else if ("SigD".equals(name)) {
            return children.get(0).toSigDeclaration();
        } else {
            // todo: other cases
            throw new NoMatchException();
        }
    }

    private Match toMatch() throws NoMatchException {
        if ("Match".equals(name)) {
            List<Pat> pats = new ArrayList<Pat>();
            int i = 0;
            try {
                children.get(i).toIdent();
                i++;
            } catch (NoMatchException ex) {
                // ignore
            }
            while (i < children.size()) {
                FakeNode child = children.get(i);
                try {
                    Pat pattern = child.toPattern();
                    pats.add(pattern);
                } catch (NoMatchException ex) {
                    break;
                }
                i++;
            }
            GRHSs grhss = toGRHSs(children.subList(i, children.size()));
            return new Match(location, pats, grhss);
        } else {
            throw new NoMatchException();
        }
    }

    private static GRHSs toGRHSs(List<FakeNode> nodes) throws NoMatchException {
        int i = 0;
        List<GRHS> grhss = new ArrayList<GRHS>();
        while (i < nodes.size()) {
            FakeNode child = nodes.get(i);
            try {
                GRHS grhs = child.toGRHS();
                grhss.add(grhs);
            } catch (NoMatchException ex) {
                break;
            }
            i++;
        }
        LocalBinds where = toLocalBinds(nodes.subList(i, nodes.size()));
        return new GRHSs(grhss, where);
    }

    private static LocalBinds toLocalBinds(List<FakeNode> nodes) throws NoMatchException {
        int i = 0;
        List<Bind> binds = new ArrayList<Bind>();
        while (i < nodes.size()) {
            FakeNode child = nodes.get(i);
            try {
                Bind bind = child.toBind();
                binds.add(bind);
            } catch (NoMatchException ex) {
                break;
            }
            i++;
        }
        List<SigDecl> sigs = new ArrayList<SigDecl>();
        while (i < nodes.size()) {
            FakeNode child = nodes.get(i);
            SigDecl sig = child.toSigDeclaration();
            sigs.add(sig);
            i++;
        }
        return new LocalBinds(binds, sigs);
    }

    private Bind toBind() throws NoMatchException {
        if ("FunBind".equals(name)) {
            Ident name = children.get(0).toIdent();
            List<Match> matches = toMatches(children.subList(1, children.size()));
            return new FunctionDecl(location, name, matches);
        } else if ("PatBind".equals(name)) {
            Pat pattern = children.get(0).toPattern();
            GRHSs grhss = toGRHSs(children.subList(1, children.size()));
            return new PatternDecl(location, pattern, grhss);
        } else {
            throw new NoMatchException();
        }
    }

    private GRHS toGRHS() throws NoMatchException {
        if ("GRHS".equals(name)) {
            for (int i = 0; i < children.size() - 1; i++) {
                Statement statement = children.get(i).toStatement();
                // todo: extract guard
            }
            Expression expression = children.get(children.size() - 1).toExpression();
            // todo: statements
            return new GRHS(location, expression);
        } else {
            throw new NoMatchException();
        }
    }

    private static List<Match> toMatches(List<FakeNode> nodes) throws NoMatchException {
        List<Match> matches = new ArrayList<Match>();
        for (FakeNode node : nodes) {
            matches.add(node.toMatch());
        }
        return matches;
    }

    private Expression toExpression() throws NoMatchException {
        if ("HsOverLit".equals(name)) {
            return new OverLiteral(location);
        } else if ("HsVar".equals(name)) {
            return new VarExpr(location);
        } else if ("HsLit".equals(name)) {
            return new Literal(location);
        } else if ("HsApp".equals(name)) {
            Expression function = children.get(0).toExpression();
            Expression arg = children.get(1).toExpression();
            return new Application(location, function, arg);
        } else if ("SectionL".equals(name)) {
            Expression arg = children.get(0).toExpression();
            Expression op = children.get(1).toExpression();
            return new LeftSection(location, arg, op);
        } else if ("SectionR".equals(name)) {
            Expression op = children.get(0).toExpression();
            Expression arg = children.get(1).toExpression();
            return new RightSection(location, op, arg);
        } else if ("OpApp".equals(name)) {
            Expression left = children.get(0).toExpression();
            Expression op = children.get(1).toExpression();
            Expression right = children.get(2).toExpression();
            return new OpApplication(location, left, op, right);
        } else if ("NegApp".equals(name)) {
            Expression right = children.get(0).toExpression();
            return new Negation(location, right);
        } else if ("HsPar".equals(name)) {
            Expression expression = children.get(0).toExpression();
            return new Parentheses(location, expression);
        } else if ("HsCase".equals(name)) {
            Expression expression = children.get(0).toExpression();
            List<Match> matches = toMatches(children.subList(1, children.size()));
            return new CaseExpr(location, expression, matches);
        } else if ("HsIf".equals(name)) {
            Expression expression = children.get(0).toExpression();
            Expression thenExpr = children.get(1).toExpression();
            Expression elseExpr = children.get(2).toExpression();
            return new IfExpr(location, expression, thenExpr, elseExpr);
        } else if ("HsLam".equals(name)) {
            List<Match> matches = toMatches(children);
            return new Lambda(location, matches);
        } else if ("ExplicitList".equals(name)) {
            List<Expression> expressions = new ArrayList<Expression>();
            for (FakeNode child : children) {
                expressions.add(child.toExpression());
            }
            return new ListExpr(location, expressions);
        } else if ("ExplicitTuple".equals(name)) {
            List<Expression> expressions = new ArrayList<Expression>();
            for (FakeNode child : children) {
                expressions.add(child.toExpression());
            }
            return new TupleExpr(location, expressions);
        } else if ("HsLet".equals(name)) {
            LocalBinds binds = toLocalBinds(children.subList(0, children.size() - 1));
            Expression expression = children.get(children.size() - 1).toExpression();
            return new LetExpr(location, binds, expression);
        } else if ("HsDo".equals(name)) {
            List<Statement> statements = new ArrayList<Statement>();
            for (int i = 0; i < children.size() - 1; i++) {
                Statement statement = children.get(i).toStatement();
                statements.add(statement);
            }
            Expression expression = children.get(children.size() - 1).toExpression();
            return new DoExpr(location, statements, expression);
        } else {
            // todo: RecordCon, RecordUpd, ExprWithTySig, ArithSeq
            // todo: other cases
            throw new NoMatchException();
        }
    }

    private Pat toPattern() throws NoMatchException {
        if ("WildPat".equals(name)) {
            return new WildPat(location);
        } else if ("VarPat".equals(name)) {
            return new VarPat(location);
        } else if ("LitPat".equals(name) || "NPat".equals(name)) {
            return new LitPat(location);
        } else if ("ParPat".equals(name)) {
            Pat pattern = children.get(0).toPattern();
            return new ParenPat(location, pattern);
        } else if ("BangPat".equals(name)) {
            Pat pattern = children.get(0).toPattern();
            return new BangPat(location, pattern);
        } else if ("LazyPat".equals(name)) {
            Pat pattern = children.get(0).toPattern();
            return new LazyPat(location, pattern);
        } else if ("ListPat".equals(name)) {
            List<Pat> patterns = new ArrayList<Pat>();
            for (FakeNode child : children) {
                patterns.add(child.toPattern());
            }
            return new ListPat(location, patterns);
        } else if ("TuplePat".equals(name)) {
            List<Pat> patterns = new ArrayList<Pat>();
            for (FakeNode child : children) {
                patterns.add(child.toPattern());
            }
            return new TuplePat(location, patterns);
        } else if ("AsPat".equals(name)) {
            Ident name = children.get(0).toIdent();
            Pat pattern = children.get(1).toPattern();
            return new AsPat(location, name, pattern);
        } else if ("ConPatIn".equals(name)) {
            Ident name = children.get(0).toIdent();
            List<Pat> patterns = new ArrayList<Pat>();
            for (int i = 1; i < children.size(); i++) {
                FakeNode child = children.get(i);
                Pat pattern = child.toPattern();
                patterns.add(pattern);
            }
            return new ConPat(location, name, new ConPatDetails(patterns)); // todo: records?
        } else {
            // todo: other cases
            throw new NoMatchException();
        }
    }

    private Statement toStatement() throws NoMatchException {
        if ("BindStmt".equals(name)) {
            Pat pattern = children.get(0).toPattern();
            Expression expression = children.get(1).toExpression();
            return new BindStmt(location, pattern, expression);
        } else if ("ExprStmt".equals(name)) {
            Expression expression = children.get(0).toExpression();
            return new ExprStmt(location, expression);
        } else if ("LetStmt".equals(name)) {
            LocalBinds binds = toLocalBinds(children);
            return new LetStmt(location, binds);
        } else {
            // todo: other cases
            throw new NoMatchException();
        }
    }

    private Type toType() throws NoMatchException {
        if ("HsForAllTy".equals(name)) {
            Type type = children.get(0).toType();
            return new ForAllType(location, type);
        } else if ("HsTyVar".equals(name)) {
            return new TyVarType(location);
        } else if ("HsAppTy".equals(name)) {
            Type type = children.get(0).toType();
            Type arg = children.get(1).toType();
            return new AppTyType(location, type, arg);
        } else if ("HsFunTy".equals(name)) {
            Type arg = children.get(0).toType();
            Type result = children.get(1).toType();
            return new FuncType(location, arg, result);
        } else if ("HsListTy".equals(name)) {
            Type type = children.get(0).toType();
            return new ListType(location, type);
        } else if ("HsTupleTy".equals(name)) {
            List<Type> types = new ArrayList<Type>();
            for (FakeNode child : children) {
                types.add(child.toType());
            }
            return new TupleType(location, types);
        } else if ("HsBangTy".equals(name)) {
            Type type = children.get(0).toType();
            return new BangType(location, type);
        } else {
            // todo: HsOpTy, HsRecTy
            // todo: other cases
            throw new NoMatchException();
        }
    }

    private SigDecl toSigDeclaration() throws NoMatchException {
        if ("TypeSig".equals(name)) {
            Ident name = children.get(0).toIdent();
            Type type = children.get(1).toType();
            return new TypeSigDecl(location, name, type);
        } else {
            // todo: other cases
            throw new NoMatchException();
        }
    }

    private TyClDecl toTyClDeclaration() throws NoMatchException {
        if ("TySynonym".equals(name)) {
            Ident name = children.get(0).toIdent();
            Type type = children.get(1).toType();
            return new TypeSynDecl(location, name, type);
        } else if ("TyData".equals(name)) {
            Ident name = children.get(0).toIdent();
            List<ConDecl> constructors = new ArrayList<ConDecl>();
            for (int i = 1; i < children.size(); i++) {
                ConDecl cons = children.get(i).toConDeclaration();
                constructors.add(cons);
            }
            return new DataTypeDecl(location, name, constructors);
        } else {
            // todo: other cases
            throw new NoMatchException();
        }
    }

    private ConDecl toConDeclaration() throws NoMatchException {
        if ("ConDecl".equals(name)) {
            Ident name = children.get(0).toIdent();
            List<Type> types = new ArrayList<Type>();
            for (int i = 1; i < children.size(); i++) {
                FakeNode child = children.get(i);
                Type type = child.toType();
                types.add(type);
            }
            return new ConDecl(location, name, types);
        } else {
            throw new NoMatchException();
        }
    }

    private Ident toIdent() throws NoMatchException {
        if ("Id".equals(name)) {
            return new Ident(location);
        } else {
            throw new NoMatchException();
        }
    }

    protected Iterable<FakeNode> getChildren() {
        return children;
    }
}
