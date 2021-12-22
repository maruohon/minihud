package fi.dy.masa.minihud.gui;

import java.util.function.Consumer;
import java.util.function.DoubleConsumer;
import java.util.function.DoubleSupplier;
import java.util.function.IntConsumer;
import java.util.function.IntSupplier;
import java.util.function.Supplier;
import net.minecraft.entity.Entity;
import net.minecraft.util.EnumFacing;
import fi.dy.masa.malilib.config.option.OptionListConfig;
import fi.dy.masa.malilib.config.value.BlockSnap;
import fi.dy.masa.malilib.gui.BaseRenderLayerEditScreen;
import fi.dy.masa.malilib.gui.BaseScreen;
import fi.dy.masa.malilib.gui.config.BaseConfigScreen;
import fi.dy.masa.malilib.gui.icon.DefaultIcons;
import fi.dy.masa.malilib.gui.listener.DoubleModifierButtonListener;
import fi.dy.masa.malilib.gui.listener.DoubleTextFieldListener;
import fi.dy.masa.malilib.gui.listener.IntegerModifierButtonListener;
import fi.dy.masa.malilib.gui.listener.IntegerTextFieldListener;
import fi.dy.masa.malilib.gui.util.GuiUtils;
import fi.dy.masa.malilib.gui.widget.BaseTextFieldWidget;
import fi.dy.masa.malilib.gui.widget.ColorIndicatorWidget;
import fi.dy.masa.malilib.gui.widget.DoubleTextFieldWidget;
import fi.dy.masa.malilib.gui.widget.IntegerTextFieldWidget;
import fi.dy.masa.malilib.gui.widget.button.GenericButton;
import fi.dy.masa.malilib.gui.widget.button.OptionListConfigButton;
import fi.dy.masa.malilib.input.ActionResult;
import fi.dy.masa.malilib.util.ListUtils;
import fi.dy.masa.malilib.util.PositionUtils;
import fi.dy.masa.malilib.util.PositionUtils.CoordinateType;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.malilib.util.consumer.DualDoubleConsumer;
import fi.dy.masa.malilib.util.consumer.DualIntConsumer;
import fi.dy.masa.malilib.util.position.CoordinateValueModifier;
import fi.dy.masa.malilib.util.position.LayerRange;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.renderer.shapes.ShapeBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeCircle;
import fi.dy.masa.minihud.renderer.shapes.ShapeCircleBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager;
import fi.dy.masa.minihud.renderer.shapes.ShapeSpawnSphere;
import fi.dy.masa.minihud.util.ShapeRenderType;

public class GuiShapeEditor extends BaseRenderLayerEditScreen
{
    private final ShapeBase shape;
    private final OptionListConfig<BlockSnap> configBlockSnap;

    public GuiShapeEditor(ShapeBase shape)
    {
        this.shape = shape;
        this.setTitle("minihud.gui.title.shape_editor");
        this.configBlockSnap = new OptionListConfig<>("blockSnap", BlockSnap.NONE, BlockSnap.VALUES, "");
    }

    @Override
    protected void initScreen()
    {
        super.initScreen();

        int x = 10;
        int y = 20;

        this.createShapeEditorElements(x, y);

        GenericButton button = new GenericButton(ConfigScreen.SHAPES.getDisplayName());
        button.setPosition(x, this.height - 24);
        button.setActionListener(() -> {
            BaseConfigScreen.setCurrentTab(Reference.MOD_ID, ConfigScreen.SHAPES);
            BaseScreen.openScreen(new GuiShapeManager());
        });
        this.addWidget(button);

        this.createLayerEditControls(146, 142, this.getLayerRange());
    }

    @Override
    protected LayerRange getLayerRange()
    {
        return this.shape.getLayerRange();
    }

    private void createColorInput(int x, int y)
    {
        this.addLabel(x, y + 1, 0xFFFFFFFF, StringUtils.translate("minihud.gui.label.color"));
        y += 12;

        BaseTextFieldWidget txtField = new BaseTextFieldWidget(70, 16, String.format("#%08X", this.shape.getColor().intValue));
        txtField.setPosition(x, y);
        txtField.setTextValidator(BaseTextFieldWidget.VALIDATOR_HEX_COLOR_8_6_4_3);
        txtField.setListener(this.shape::setColorFromString);
        this.addWidget(txtField);
        this.nextY = y + 20;

        ColorIndicatorWidget ci = new ColorIndicatorWidget(18, 18, this.shape.getColor().intValue, this.shape::setColor);
        ci.setPosition(x + 74, y - 1);
        this.addWidget(ci);
    }

    private void createShapeEditorElements(int x, int y)
    {
        this.addLabel(x, y + 1, 0xFFFFFFFF, StringUtils.translate("minihud.gui.label.display_name_colon"));
        y += 12;

        BaseTextFieldWidget textField = new BaseTextFieldWidget(240, 16, this.shape.getDisplayName());
        textField.setPosition(x, y);
        textField.setListener(this.shape::setDisplayName);
        this.addWidget(textField);
        y += 20;

        int renderTypeX = x + 230;
        int renderTypeY = y + 2;

        switch (this.shape.getType())
        {
            case CAN_DESPAWN_SPHERE:
            case CAN_SPAWN_SPHERE:
            case DESPAWN_SPHERE:
            {
                ShapeSpawnSphere shape = (ShapeSpawnSphere) this.shape;
                this.createShapeEditorElementsSphereBase(x, y, false);
                this.createShapeEditorElementDoubleField(x + 150, y + 2, shape::getMargin, shape::setMargin, "minihud.gui.label.margin_colon", false);
                break;
            }

            case CIRCLE:
            {
                ShapeCircle shape = (ShapeCircle) this.shape;
                this.createShapeEditorElementsSphereBase(x, y, true);
                this.createShapeEditorElementIntField(x + 150, y + 36, shape::getHeight, shape::setHeight, "minihud.gui.label.height_colon", true);
                this.createDirectionButton(x + 230, y + 36, shape::getMainAxis, shape::setMainAxis, "minihud.gui.label.circle.main_axis_colon");
                this.createRenderTypeButton(renderTypeX, renderTypeY, this.shape::getRenderType, this.shape::setRenderType, "minihud.gui.label.render_type_colon");
                break;
            }

            case SPHERE_BLOCKY:
                this.createShapeEditorElementsSphereBase(x, y, true);
                this.createRenderTypeButton(renderTypeX, renderTypeY, this.shape::getRenderType, this.shape::setRenderType, "minihud.gui.label.render_type_colon");
                break;
        }
    }

    private void createShapeEditorElementsSphereBase(int x, int y, boolean addRadiusInput)
    {
        ShapeCircleBase shape = (ShapeCircleBase) this.shape;

        this.addLabel(x, y + 2, 0xFFFFFFFF, StringUtils.translate("minihud.gui.label.center_colon"));

        if (addRadiusInput)
        {
            this.createShapeEditorElementDoubleField(x + 150, y + 2, shape::getRadius, shape::setRadius, "minihud.gui.label.radius_colon", true);
        }

        y += 12;
        GuiUtils.createVec3dInputsVertical(x, y, 120, shape.getCenter(), new SphereEditor(shape, this), true, this);
        x += 11;
        y += 54;

        GenericButton button = new GenericButton("malilib.gui.button.render_layers_gui.set_to_player");
        button.setPosition(x, y);
        button.setActionListener(() -> {
            Entity entity = this.mc.getRenderViewEntity();

            if (entity != null)
            {
                shape.setCenter(entity.getPositionVector());
                this.initGui();
            }
        });
        this.addWidget(button);

        this.configBlockSnap.setValue(shape.getBlockSnap());
        int bx = x + button.getWidth() + 4;

        OptionListConfigButton buttonSnap = new OptionListConfigButton(-1, 20, this.configBlockSnap, "minihud.gui.label.shape.block_snap");
        buttonSnap.setPosition(bx, y);
        buttonSnap.setActionListener(() -> {
            shape.setBlockSnap(this.configBlockSnap.getValue());
            this.initGui();
        });
        this.addWidget(buttonSnap);

        y += 24;

        this.createColorInput(x, y);
    }

    private void createShapeEditorElementDoubleField(int x, int y, DoubleSupplier supplier,
                                                     DoubleConsumer consumer, String translationKey, boolean addButton)
    {
        this.addLabel(x + 12, y, 0xFFFFFFFF, translationKey);
        y += 10;

        DoubleTextFieldWidget txtField = new DoubleTextFieldWidget(40, 16, supplier.getAsDouble());
        txtField.setPosition(x + 12, y);
        txtField.setListener(new DoubleTextFieldListener(consumer));
        txtField.setUpdateListenerAlways(true);
        this.addWidget(txtField);

        if (addButton)
        {
            String hover = StringUtils.translate("malilib.gui.button.hover.plus_minus_tip");
            GenericButton button = GenericButton.createIconOnly(() -> DefaultIcons.BTN_PLUSMINUS_16);
            button.setPosition(x + 54, y);
            button.setActionListener(new DoubleModifierButtonListener(supplier, new DualDoubleConsumer(consumer, (val) -> txtField.setText(String.valueOf(supplier.getAsDouble())) )));
            button.setCanScrollToClick(true);
            button.addHoverStrings(hover);
            this.addWidget(button);
        }
    }

    private void createShapeEditorElementIntField(int x, int y, IntSupplier supplier, IntConsumer consumer,
                                                  String translationKey, boolean addButton)
    {
        this.addLabel(x + 12, y, 0xFFFFFFFF, translationKey);
        y += 10;

        IntegerTextFieldWidget txtField = new IntegerTextFieldWidget(40, 16, supplier.getAsInt());
        txtField.setPosition(x + 12, y);
        txtField.setListener(new IntegerTextFieldListener(consumer));
        txtField.setUpdateListenerAlways(true);
        this.addWidget(txtField);

        if (addButton)
        {
            String hover = StringUtils.translate("malilib.gui.button.hover.plus_minus_tip");
            GenericButton button = GenericButton.createIconOnly(() -> DefaultIcons.BTN_PLUSMINUS_16);
            button.setPosition(x + 54, y);
            button.setActionListener(new IntegerModifierButtonListener(supplier, new DualIntConsumer(consumer, (val) -> txtField.setText(String.valueOf(supplier.getAsInt())) )));
            button.setCanScrollToClick(true);
            button.addHoverStrings(hover);
            this.addWidget(button);
        }
    }

    private void createDirectionButton(int x, int y, Supplier<EnumFacing> supplier,
                                       Consumer<EnumFacing> consumer, String translationKey)
    {
        this.addLabel(x, y, 0xFFFFFFFF, translationKey);
        y += 10;

        String name = org.apache.commons.lang3.StringUtils.capitalize(supplier.get().toString().toLowerCase());
        GenericButton button = new GenericButton(50, 20, name);
        button.setPosition(x, y);
        button.setActionListener((btn) -> { consumer.accept(PositionUtils.cycleDirection(supplier.get(), btn == 1)); this.initGui(); return true; });
        this.addWidget(button);
    }

    private void createRenderTypeButton(int x, int y, Supplier<ShapeRenderType> supplier,
                                        Consumer<ShapeRenderType> consumer, String translationKey)
    {
        this.addLabel(x, y, 0xFFFFFFFF, translationKey);
        y += 10;

        GenericButton button = new GenericButton(supplier.get().getDisplayName());
        button.setPosition(x, y);
        button.setActionListener((btn) -> { consumer.accept(ListUtils.getNextEntry(ShapeRenderType.VALUES, supplier.get(), btn != 0)); this.initGui(); return true; });
        this.addWidget(button);
    }

    private static class SphereEditor implements CoordinateValueModifier
    {
        private final GuiShapeEditor gui;
        private final ShapeCircleBase shape;

        private SphereEditor(ShapeCircleBase shape, GuiShapeEditor gui)
        {
            this.shape = shape;
            this.gui = gui;
        }

        @Override
        public boolean modifyValue(CoordinateType type, int amount)
        {
            this.shape.setCenter(PositionUtils.modifyValue(type, this.shape.getCenter(), amount));
            this.gui.initGui();
            return true;
        }

        @Override
        public boolean setValueFromString(CoordinateType type, String newValue)
        {
            try
            {
                this.shape.setCenter(PositionUtils.setValue(type, this.shape.getCenter(), Double.parseDouble(newValue)));
                return true;
            }
            catch (Exception ignore) {}

            return false;
        }
    }

    public static ActionResult openShapeEditor()
    {
        ShapeBase shape = ShapeManager.INSTANCE.getSelectedShape();
        BaseScreen screen = shape != null ? new GuiShapeEditor(shape) : GuiShapeManager.openShapeManager(null);
        BaseScreen.openScreen(screen);
        return ActionResult.SUCCESS;
    }
}
