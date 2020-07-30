package fi.dy.masa.minihud.gui;

import java.util.function.Consumer;
import java.util.function.DoubleConsumer;
import java.util.function.DoubleSupplier;
import java.util.function.IntConsumer;
import java.util.function.IntSupplier;
import java.util.function.Supplier;
import org.lwjgl.input.Keyboard;
import net.minecraft.entity.Entity;
import net.minecraft.util.EnumFacing;
import fi.dy.masa.malilib.config.option.OptionListConfig;
import fi.dy.masa.malilib.config.value.BlockSnap;
import fi.dy.masa.malilib.gui.GuiRenderLayerEditBase;
import fi.dy.masa.malilib.gui.button.ButtonGeneric;
import fi.dy.masa.malilib.gui.button.ConfigButtonOptionList;
import fi.dy.masa.malilib.gui.listener.ButtonListenerDoubleModifier;
import fi.dy.masa.malilib.gui.listener.ButtonListenerIntModifier;
import fi.dy.masa.malilib.gui.listener.TextFieldListenerDouble;
import fi.dy.masa.malilib.gui.listener.TextFieldListenerInteger;
import fi.dy.masa.malilib.gui.util.BaseGuiIcon;
import fi.dy.masa.malilib.gui.util.GuiUtils;
import fi.dy.masa.malilib.gui.widget.WidgetColorIndicator;
import fi.dy.masa.malilib.gui.widget.WidgetTextFieldBase;
import fi.dy.masa.malilib.gui.widget.WidgetTextFieldDouble;
import fi.dy.masa.malilib.gui.widget.WidgetTextFieldInteger;
import fi.dy.masa.malilib.util.position.ICoordinateValueModifier;
import fi.dy.masa.malilib.util.consumer.DualDoubleConsumer;
import fi.dy.masa.malilib.util.consumer.DualIntConsumer;
import fi.dy.masa.malilib.util.position.LayerRange;
import fi.dy.masa.malilib.util.PositionUtils;
import fi.dy.masa.malilib.util.PositionUtils.CoordinateType;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.renderer.shapes.ShapeBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeCircle;
import fi.dy.masa.minihud.renderer.shapes.ShapeCircleBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeSpawnSphere;
import fi.dy.masa.minihud.util.ShapeRenderType;

public class GuiShapeEditor extends GuiRenderLayerEditBase
{
    private final ShapeBase shape;
    private OptionListConfig<BlockSnap> configBlockSnap;
    private int colorY;

    public GuiShapeEditor(ShapeBase shape)
    {
        this.shape = shape;
        this.title = StringUtils.translate("minihud.gui.title.shape_editor");
        this.configBlockSnap = new OptionListConfig<BlockSnap>("blockSnap", BlockSnap.NONE, "");
    }

    @Override
    public void initGui()
    {
        super.initGui();

        int x = 10;
        int y = 20;

        this.createShapeEditorElements(x, y);

        ButtonGeneric button = new ButtonGeneric(x, this.height - 24, -1, 20, GuiConfigs.SHAPES.getDisplayName());
        this.addButton(button, new GuiShapeManager.ButtonListenerTab(GuiConfigs.SHAPES));

        this.createLayerEditControls(146, 142, this.getLayerRange());

        Keyboard.enableRepeatEvents(true);
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

        WidgetTextFieldBase txtField = new WidgetTextFieldBase(x, y, 70, 17, String.format("#%08X", this.shape.getColor().intValue));
        txtField.setTextValidator(WidgetTextFieldBase.VALIDATOR_HEX_COLOR_8);
        txtField.setListener((txt) -> this.shape.setColorFromString(txt));
        this.addWidget(txtField);
        this.nextY = y + 20;
        this.colorY = y - 1;

        this.addWidget(new WidgetColorIndicator(x + 74, this.colorY, 19, 19, this.shape.getColor(), (val) -> this.shape.setColor(val) ));
    }

    private void createShapeEditorElements(int x, int y)
    {
        this.addLabel(x, y + 1, 0xFFFFFFFF, StringUtils.translate("minihud.gui.label.display_name_colon"));
        y += 12;

        WidgetTextFieldBase textField = new WidgetTextFieldBase(x, y, 240, 17, this.shape.getDisplayName());
        textField.setListener((txt) -> this.shape.setDisplayName(txt));
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
                this.createShapeEditorElementDoubleField(x + 150, y + 2, () -> shape.getMargin(), (val) -> shape.setMargin(val), "minihud.gui.label.margin_colon", false);
                break;
            }

            case CIRCLE:
            {
                ShapeCircle shape = (ShapeCircle) this.shape;
                this.createShapeEditorElementsSphereBase(x, y, true);
                this.createShapeEditorElementIntField(x + 150, y + 36, () -> shape.getHeight(), (val) -> shape.setHeight(val), "minihud.gui.label.height_colon", true);
                this.createDirectionButton(x + 230, y + 36, () -> shape.getMainAxis(), (val) -> shape.setMainAxis(val), "minihud.gui.label.circle.main_axis_colon");
                this.createRenderTypeButton(renderTypeX, renderTypeY, () -> this.shape.getRenderType(), (val) -> this.shape.setRenderType(val), "minihud.gui.label.render_type_colon");
                break;
            }

            case SPHERE_BLOCKY:
                this.createShapeEditorElementsSphereBase(x, y, true);
                this.createRenderTypeButton(renderTypeX, renderTypeY, () -> this.shape.getRenderType(), (val) -> this.shape.setRenderType(val), "minihud.gui.label.render_type_colon");
                break;
        }
    }

    private void createShapeEditorElementsSphereBase(int x, int y, boolean addRadiusInput)
    {
        ShapeCircleBase shape = (ShapeCircleBase) this.shape;

        this.addLabel(x, y + 2, 0xFFFFFFFF, StringUtils.translate("minihud.gui.label.center_colon"));

        if (addRadiusInput)
        {
            this.createShapeEditorElementDoubleField(x + 150, y + 2, () -> shape.getRadius(), (val) -> shape.setRadius(val), "minihud.gui.label.radius_colon", true);
        }

        y += 12;
        GuiUtils.createVec3dInputsVertical(x, y, 120, shape.getCenter(), new SphereEditor(shape, this), true, this);
        x += 11;
        y += 54;

        ButtonGeneric button = new ButtonGeneric(x, y, -1, false, "malilib.gui.button.render_layers_gui.set_to_player");
        this.addButton(button, (btn, mbtn) -> {
            Entity entity = this.mc.getRenderViewEntity();

            if (entity != null)
            {
                shape.setCenter(entity.getPositionVector());
                this.initGui();
            }
        });

        this.configBlockSnap.setOptionListValue(shape.getBlockSnap());
        ConfigButtonOptionList buttonSnap = new ConfigButtonOptionList(x + button.getWidth() + 4, y, -1, 20, this.configBlockSnap, "minihud.gui.label.shape.block_snap");
        this.addButton(buttonSnap, (btn, mbtn) -> {
            shape.setBlockSnap(this.configBlockSnap.getOptionListValue());
            this.initGui();
        });

        y += 24;

        this.createColorInput(x, y);
    }

    private void createShapeEditorElementDoubleField(int x, int y, DoubleSupplier supplier, DoubleConsumer consumer, String translationKey, boolean addButton)
    {
        this.addLabel(x + 12, y, 0xFFFFFFFF, translationKey);
        y += 11;

        WidgetTextFieldDouble txtField = new WidgetTextFieldDouble(x + 12, y, 40, 14, supplier.getAsDouble());
        txtField.setListener(new TextFieldListenerDouble(consumer));
        txtField.setUpdateListenerAlways(true);
        this.addWidget(txtField);

        if (addButton)
        {
            String hover = StringUtils.translate("malilib.gui.button.hover.plus_minus_tip");
            ButtonGeneric button = new ButtonGeneric(x + 54, y - 1, BaseGuiIcon.BTN_PLUSMINUS_16, hover);
            this.addButton(button, new ButtonListenerDoubleModifier(supplier, new DualDoubleConsumer(consumer, (val) -> txtField.setText(String.valueOf(supplier.getAsDouble())) )));
        }
    }

    private void createShapeEditorElementIntField(int x, int y, IntSupplier supplier, IntConsumer consumer, String translationKey, boolean addButton)
    {
        this.addLabel(x + 12, y, 0xFFFFFFFF, translationKey);
        y += 11;

        WidgetTextFieldInteger txtField = new WidgetTextFieldInteger(x + 12, y, 40, 14, supplier.getAsInt());
        txtField.setListener(new TextFieldListenerInteger(consumer));
        txtField.setUpdateListenerAlways(true);
        this.addWidget(txtField);

        if (addButton)
        {
            String hover = StringUtils.translate("malilib.gui.button.hover.plus_minus_tip");
            ButtonGeneric button = new ButtonGeneric(x + 54, y - 1, BaseGuiIcon.BTN_PLUSMINUS_16, hover);
            this.addButton(button, new ButtonListenerIntModifier(supplier, new DualIntConsumer(consumer, (val) -> txtField.setText(String.valueOf(supplier.getAsInt())) )));
        }
    }

    private void createDirectionButton(int x, int y, Supplier<EnumFacing> supplier, Consumer<EnumFacing> consumer, String translationKey)
    {
        this.addLabel(x, y, 0xFFFFFFFF, translationKey);
        y += 10;

        ButtonGeneric button = new ButtonGeneric(x, y, 50, 20, org.apache.commons.lang3.StringUtils.capitalize(supplier.get().toString().toLowerCase()));
        this.addButton(button, (btn, mouseBtn) -> { consumer.accept(PositionUtils.cycleDirection(supplier.get(), mouseBtn == 1)); this.initGui(); } );
    }

    private void createRenderTypeButton(int x, int y, Supplier<ShapeRenderType> supplier, Consumer<ShapeRenderType> consumer, String translationKey)
    {
        this.addLabel(x, y, 0xFFFFFFFF, translationKey);
        y += 10;

        ButtonGeneric button = new ButtonGeneric(x, y, -1, 20, supplier.get().getDisplayName());
        this.addButton(button, (btn, mouseBtn) -> { consumer.accept(supplier.get().cycle(mouseBtn == 0)); this.initGui(); } );
    }

    private static class SphereEditor implements ICoordinateValueModifier
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
            catch (Exception e) {}

            return false;
        }
    }
}
