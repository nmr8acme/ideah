package ideah.sdk;

import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkModel;
import com.intellij.openapi.projectRoots.SdkModificator;
import com.intellij.openapi.roots.OrderRootType;
import com.intellij.openapi.util.Comparing;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.Map;

public class HaskellSdkConfigurableForm {

//    private JComboBox myInternalJdkComboBox;
    private JComboBox myGhcLibComboBox;
    private JPanel myContentPanel;
//    private JComboBox myBuildTargetComboBox;
    private JComboBox myCabalComboBox;
    private JTextField myGhcOptionsTextField;

    private final DefaultComboBoxModel myGhcLibModel = new DefaultComboBoxModel();
    private final SdkModel mySdkModel;

    private final DefaultComboBoxModel myCabalModel = new DefaultComboBoxModel();
    private String mySdkLocation;

    public HaskellSdkConfigurableForm(@NotNull SdkModel sdkModel, @NotNull final SdkModificator sdkModificator) {
        mySdkModel = sdkModel;
        myGhcLibComboBox.setModel(myGhcLibModel);
        myGhcLibComboBox.setRenderer(new DefaultListCellRenderer() {
            public Component getListCellRendererComponent(final JList list,
                                                          final Object value,
                                                          final int index, final boolean isSelected, final boolean cellHasFocus) {
                final Component rendererComponent = super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
                if (value instanceof Sdk) {
                    setText(((Sdk) value).getName());
                }
                return rendererComponent;
            }
        });
        myCabalComboBox.setModel(myCabalModel);

        myCabalComboBox.setRenderer(new DefaultListCellRenderer() {
            public Component getListCellRendererComponent(final JList list,
                                                          final Object value,
                                                          final int index, final boolean isSelected, final boolean cellHasFocus) {
                final Component rendererComponent = super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
//          if (value instanceof IAndroidTarget) {
//            setText(AndroidSdkUtils.getTargetPresentableName((IAndroidTarget)value));
//          }
                setText("test text");
                return rendererComponent;
            }
        });

        myCabalComboBox.addItemListener(new ItemListener() {
            public void itemStateChanged(final ItemEvent e) {
//          final IAndroidTarget target = (IAndroidTarget)e.getItem();
//
//          java.util.List<OrderRoot> roots = AndroidSdkUtils.getLibraryRootsForTarget(target, mySdkLocation);
                Map<OrderRootType, VirtualFile[]> configuredRoots = new com.intellij.util.containers.HashMap<OrderRootType, VirtualFile[]>();

                for (OrderRootType type : OrderRootType.getAllTypes()) {
                    configuredRoots.put(type, sdkModificator.getRoots(type));
                }

//          for (OrderRoot root : roots) {
//            if (e.getStateChange() == ItemEvent.DESELECTED) {
//              sdkModificator.removeRoot(root.getFile(), root.getType());
//            }
//            else {
//              VirtualFile[] configuredRootsForType = configuredRoots.get(root.getType());
//              if (ArrayUtil.find(configuredRootsForType, root.getFile()) == -1) {
//                sdkModificator.addRoot(root.getFile(), root.getType());
//              }
//            }
//          }
            }
        });
    }

    @NotNull
    public JPanel getContentPanel() {
        return myContentPanel;
    }

    @Nullable
    public Sdk getSelectedSdk() {
        return (Sdk) myGhcLibComboBox.getSelectedItem();
    }

    @Nullable
//    public IAndroidTarget getSelectedBuildTarget() {
//      return (IAndroidTarget)myCabalComboBox.getSelectedItem();
//    }

    public void init(Sdk jdk, Sdk androidSdk, Object buildTarget) {
        updateJdks();

        if (androidSdk != null) {
            for (int i = 0; i < myGhcLibModel.getSize(); i++) {
                if (Comparing.strEqual(((Sdk) myGhcLibModel.getElementAt(i)).getName(), jdk.getName())) {
                    myGhcLibComboBox.setSelectedIndex(i);
                    break;
                }
            }
        }

        mySdkLocation = androidSdk != null ? androidSdk.getHomePath() : null;
//      AndroidSdk androidSdkObject = mySdkLocation != null ? AndroidSdk.parse(mySdkLocation, new EmptySdkLog()) : null;
//      updateBuildTargets(androidSdkObject);

//      if (buildTarget != null) {
//        for (int i = 0; i < myCabalModel.getSize(); i++) {
//          IAndroidTarget target = (IAndroidTarget)myCabalModel.getElementAt(i);
//          if (buildTarget.hashString().equals(target.hashString())) {
//            myCabalComboBox.setSelectedIndex(i);
//            break;
//          }
//        }
//      }
    }

    private void updateJdks() {
        myGhcLibModel.removeAllElements();
        for (Sdk sdk : mySdkModel.getSdks()) {
//        if (AndroidSdkUtils.isApplicableJdk(sdk)) {
            myGhcLibModel.addElement(sdk);
//        }
        }
    }

//    private void updateBuildTargets(AndroidSdk androidSdk) {
//      myCabalModel.removeAllElements();
//
//      if (androidSdk != null) {
//        for (IAndroidTarget target : androidSdk.getTargets()) {
//          myCabalModel.addElement(target);
//        }
//      }
//    }

    public void addJavaSdk(Sdk sdk) {
        myGhcLibModel.addElement(sdk);
    }

    public void removeJavaSdk(Sdk sdk) {
        myGhcLibModel.removeElement(sdk);
    }

    public void updateJdks(Sdk sdk, String previousName) {
        final Sdk[] sdks = mySdkModel.getSdks();
//      for (Sdk currentSdk : sdks) {
//        if (currentSdk.getSdkType().equals(AndroidSdkType.getInstance())) {
//          final AndroidSdkAdditionalData data = (AndroidSdkAdditionalData)currentSdk.getSdkAdditionalData();
//          final Sdk internalJava = data != null ? data.getJavaSdk() : null;
//          if (internalJava != null && Comparing.equal(internalJava.getName(), previousName)) {
//            data.setJavaSdk(sdk);
//          }
//        }
//      }
        updateJdks();
    }

    public void internalJdkUpdate(final Sdk sdk) {
//      AndroidSdkAdditionalData data = (AndroidSdkAdditionalData)sdk.getSdkAdditionalData();
//      if (data == null) return;
//      final Sdk javaSdk = data.getJavaSdk();
//      if (myGhcLibModel.getIndexOf(javaSdk) == -1) {
//        myGhcLibModel.addElement(javaSdk);
//      }
//      else {
//        myGhcLibModel.setSelectedItem(javaSdk);
//      }
    }


}
