package ideah.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.progress.ProcessCanceledException;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiReference;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.IncorrectOperationException;
import ideah.lexer.HaskellTokenType;
import ideah.lexer.HaskellTokenTypes;
import ideah.lexer.LexedIdentifier;
import ideah.parser.HaskellElementTypes;
import ideah.psi.api.HPIdent;
import ideah.psi.api.util.HaskellPsiElementFactory;
import ideah.util.DeclarationPosition;
import ideah.util.LineCol;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class HPIdentImpl extends HaskellBaseElementImpl implements HPIdent, PsiReference {

    private static final Logger LOG = Logger.getInstance("ideah.psi.impl.HPIdentImpl");

    public HPIdentImpl(@NotNull ASTNode node) {
        super(node);
    }

    @Override
    public PsiReference getReference() {
        return this;
    }

    @Nullable
    public PsiElement setName(@NotNull @NonNls String name) throws IncorrectOperationException {
        LexedIdentifier parsedNewName = LexedIdentifier.parse(name);
        LexedIdentifier parsedOldName = LexedIdentifier.parse(getText());
        LOG.assertTrue(!(parsedNewName == null || parsedOldName == null));
        boolean newNameOperator = HaskellTokenTypes.OPERATORS.contains(parsedNewName.type);
        String createdNewName;
        ASTNode parentNode = getParent().getNode();
        ASTNode grandParentNode = parentNode.getTreeParent();
        HaskellTokenType infixPrefixIdentType = HaskellElementTypes.INFIX_PREFIX_IDENT;
        IElementType parentType = grandParentNode.getElementType() == infixPrefixIdentType ? infixPrefixIdentType : parentNode.getElementType();
        boolean isPrefixInfixIdent = parentType == infixPrefixIdentType;
        boolean oldOrNewOperatorAndOneOfThemNot = !newNameOperator == HaskellTokenTypes.OPERATORS.contains(parsedOldName.type);

        if (oldOrNewOperatorAndOneOfThemNot != isPrefixInfixIdent) {
            if (newNameOperator) {
                createdNewName = "(" + name + ")";
            } else {
                createdNewName = "`" + name + "`";
            }
        } else {
            createdNewName = name;
        }

        String module = parsedOldName.module;
        String newName = module == null ? createdNewName : module + "." + createdNewName;
        HaskellPsiElementFactory factory = HaskellPsiElementFactory.getInstance(getProject());
        ASTNode newIdentNode = factory.createIdentNodeFromText(newName);
        if (newIdentNode == null)
            return this;
        ASTNode nodeToBeReplaced = isPrefixInfixIdent ? grandParentNode : parentNode;
        ASTNode nodeToReplace = isPrefixInfixIdent ? getParent().getNode() : getNode();
        nodeToBeReplaced.replaceChild(nodeToReplace, newIdentNode);
        return newIdentNode.getPsi();
    }

    public PsiElement getElement() {
        return this;
    }

    public TextRange getRangeInElement() {
        return new TextRange(0, getTextLength());
    }

    public PsiElement resolve() {
        PsiFile psiFile = getContainingFile();
        LineCol coord = LineCol.fromOffset(psiFile, getTextOffset());
        try {
            DeclarationPosition declaration = DeclarationPosition.get(psiFile, coord);
            return getElementAt(getProject(), declaration);
        } catch (ProcessCanceledException ex) {
            return null;
        } catch (Exception e) {
            LOG.error(e);
            return null;
        }
    }

    public static HPIdentImpl getElementAt(Project project, DeclarationPosition declaration) {
        if (declaration == null)
            return null;
        VirtualFile baseDir = project.getBaseDir();
        if (baseDir == null)
            return null;
        VirtualFile declarationModuleVirtualFile = baseDir.getFileSystem().findFileByPath(declaration.module);
        if (declarationModuleVirtualFile == null)
            return null;
        PsiFile declarationModulePsiFile = PsiManager.getInstance(project).findFile(declarationModuleVirtualFile);
        if (declarationModulePsiFile == null)
            return null;
        int declarationStart = declaration.coord.getOffset(declarationModulePsiFile);
        PsiElement elementAt = declarationModulePsiFile.findElementAt(declarationStart);
        if (elementAt == null)
            return null;
        return new HPIdentImpl(elementAt.getNode());
    }

    @NotNull
    public String getCanonicalText() {
        return getText(); // todo: resolve full module name?
    }

    public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
        return setName(newElementName);
    }

    public PsiElement bindToElement(@NotNull PsiElement element) throws IncorrectOperationException {
        return null; // todo
    }

    public boolean isReferenceTo(PsiElement element) {
        return false; // todo
    }

    @NotNull
    public Object[] getVariants() {
        return new Object[0]; // todo
    }

    public boolean isSoft() {
        return false;
    }
}
