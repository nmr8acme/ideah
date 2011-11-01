package ideah.findUsages;

import com.intellij.openapi.application.QueryExecutorBase;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.util.Processor;
import ideah.psi.impl.HPIdentImpl;
import ideah.util.CompilerLocation;
import ideah.util.DeclarationPosition;
import ideah.util.LineCol;
import ideah.util.ProcessLauncher;
import org.jetbrains.annotations.NotNull;

import java.io.BufferedReader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

public final class HaskellReferencesSearch extends QueryExecutorBase<PsiReference, ReferencesSearch.SearchParameters> {

    private static final Logger LOG = Logger.getInstance("ideah.findUsages.HaskellReferencesSearch");

    public HaskellReferencesSearch() {
        super(true);
    }

    @Override
    public void processQuery(@NotNull ReferencesSearch.SearchParameters queryParameters, @NotNull Processor<PsiReference> consumer) {
        PsiElement element = queryParameters.getElementToSearch();
        if (element instanceof HPIdentImpl) {
            HPIdentImpl ident = (HPIdentImpl) element;
            PsiFile file = element.getContainingFile();
            try {
                int textOffset = element.getTextOffset();
                DeclarationPosition declaration = DeclarationPosition.get(file, LineCol.fromOffset(file, textOffset));
                LineCol coord = declaration.coord;
                VirtualFile virtualFile = file.getVirtualFile();
                Project project = file.getProject();
                Module module = ProjectRootManager.getInstance(project).getFileIndex().getModuleForFile(virtualFile);
                CompilerLocation compiler = CompilerLocation.get(module);
                ProcessLauncher launcher = new ProcessLauncher(true, null,
                    compiler.exe,
                    "-m", "FindUsages",
                    "-g", compiler.libPath,
                    "-s", CompilerLocation.rootsAsString(module, false),
                    "--line-number", String.valueOf(coord.line), "--column-number", String.valueOf(coord.column),
                    virtualFile.getPath()
                );
                BufferedReader bf = new BufferedReader(new StringReader(launcher.getStdOut()));
                String line = bf.readLine();
                while (line != null) {
                    LineCol refLineCol = LineCol.parse(line);
                    line = bf.readLine();
                    if (line != null) {
                        PsiElement elementAt = HPIdentImpl.getElementAt(project, new DeclarationPosition(refLineCol, LineCol.cleanString(line)));
                        PsiReference reference = elementAt.getReference();
                        consumer.process(reference);
                    }
                }
            } catch (Exception e) {
                LOG.error(e);
            }
            consumer.process(ident);
            //GroovyConstructorUsagesSearcher.processConstructorUsages(ident, queryParameters.getScope(), consumer, queryParameters.getOptimizer(), true, false);
        }
    }
}
