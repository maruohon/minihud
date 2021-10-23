package fi.dy.masa.minihud.gui;

import java.util.function.Consumer;
import java.util.function.DoubleConsumer;
import java.util.function.DoubleSupplier;
import java.util.function.IntConsumer;
import java.util.function.IntSupplier;
import java.util.function.Supplier;
import com.google.common.collect.ImmutableList;
import net.minecraft.client.util.math.MatrixStack;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.Direction;
import fi.dy.masa.malilib.config.IConfigInteger;
import fi.dy.masa.malilib.config.options.ConfigInteger;
import fi.dy.masa.malilib.config.options.ConfigOptionList;
import fi.dy.masa.malilib.gui.GuiBase;
import fi.dy.masa.malilib.gui.GuiColorEditorHSV;
import fi.dy.masa.malilib.gui.GuiRenderLayerEditBase;
import fi.dy.masa.malilib.gui.GuiTextFieldDouble;
import fi.dy.masa.malilib.gui.GuiTextFieldGeneric;
import fi.dy.masa.malilib.gui.GuiTextFieldInteger;
import fi.dy.masa.malilib.gui.MaLiLibIcons;
import fi.dy.masa.malilib.gui.button.ButtonBase;
import fi.dy.masa.malilib.gui.button.ButtonGeneric;
import fi.dy.masa.malilib.gui.button.ConfigButtonOptionList;
import fi.dy.masa.malilib.gui.button.IButtonActionListener;
import fi.dy.masa.malilib.gui.interfaces.ITextFieldListener;
import fi.dy.masa.malilib.gui.widgets.WidgetBase;
import fi.dy.masa.malilib.interfaces.ICoordinateValueModifier;
import fi.dy.masa.malilib.render.RenderUtils;
import fi.dy.masa.malilib.util.BlockSnap;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.malilib.util.GuiUtils;
import fi.dy.masa.malilib.util.LayerRange;
import fi.dy.masa.malilib.util.PositionUtils;
import fi.dy.masa.malilib.util.PositionUtils.CoordinateType;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.gui.GuiConfigs.ConfigGuiTab;
import fi.dy.masa.minihud.renderer.shapes.ShapeBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeCircle;
import fi.dy.masa.minihud.renderer.shapes.ShapeCircleBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeSpawnSphere;
import fi.dy.masa.minihud.util.ShapeRenderType;

public class GuiShapeEditor extends GuiRenderLayerEditBase
{
    private final ShapeBase shape;
    private ConfigOptionList configBlockSnap;
    private int colorY;

    public GuiShapeEditor(ShapeBase shape)
    {
        this.shape = shape;
        this.title = StringUtils.translate("minihud.gui.title.shape_editor");
        this.configBlockSnap = new ConfigOptionList("blockSnap", BlockSnap.NONE, "");
    }

    @Override
    public void initGui()
    {
        super.initGui();

        int x = 10;
        int y = 20;

        this.createShapeEditorElements(x, y);

        ButtonGeneric button = new ButtonGeneric(x, this.height - 24, -1, 20, ConfigGuiTab.SHAPES.getDisplayName());
        this.addButton(button, new GuiShapeManager.ButtonListenerTab(ConfigGuiTab.SHAPES));

        this.createLayerEditControls(146, 162, this.getLayerRange());
    }

    @Override
    protected LayerRange getLayerRange()
    {
        return this.shape.getLayerRange();
    }

    private void createColorInput(int x, int y)
    {
        this.addLabel(x, y, -1, 14, 0xFFFFFFFF, StringUtils.translate("minihud.gui.label.color"));
        y += 12;

        GuiTextFieldGeneric textField = new GuiTextFieldGeneric(x, y, 70, 17, this.textRenderer);
        textField.setMaxLength(12);
        textField.setText(String.format("#%08X", this.shape.getColor().intValue));
        this.addTextField(textField, new TextFieldListenerColor(this.shape));
        this.nextY = y + 20;
        this.colorY = y - 1;

        this.addWidget(new WidgetColorIndicator(x + 74, this.colorY, 19, 19, this.shape.getColor(), this.shape::setColor));
    }

    private void createShapeEditorElements(int x, int y)
    {
        this.addLabel(x, y, -1, 14, 0xFFFFFFFF, StringUtils.translate("minihud.gui.label.display_name_colon"));
        y += 12;

        GuiTextFieldGeneric textField = new GuiTextFieldGeneric(x, y, 240, 17, this.textRenderer);
        textField.setText(this.shape.getDisplayName());
        this.addTextField(textField, (txtFld) -> { this.shape.setDisplayName(txtFld.getText()); return true; });
        y += 20;

        int renderTypeX = x + 230;
        int renderTypeY = y + 2;

        switch (this.shape.getType())
        {
            case ADJUSTABLE_SPAWN_SPHERE:
            {
                this.createShapeEditorElementsSphereBase(x, y, true);
                break;
            }

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

        this.addLabel(x, y, 60, 14, 0xFFFFFFFF, StringUtils.translate("minihud.gui.label.center_colon"));

        if (addRadiusInput)
        {
            this.createShapeEditorElementDoubleField(x + 150, y + 2, shape::getRadius, shape::setRadius, "minihud.gui.label.radius_colon", true);
        }

        y += 12;
        GuiUtils.createVec3dInputsVertical(x, y, 120, shape.getCenter(), new SphereEditor(shape, this), true, this);
        x += 11;
        y += 54;

        ButtonGeneric button = new ButtonGeneric(x, y, -1, false, "malilib.gui.button.render_layers_gui.set_to_player");
        this.addButton(button, new ButtonListenerSphere(shape, this));

        this.configBlockSnap.setOptionListValue(shape.getBlockSnap());
        String label = StringUtils.translate("minihud.gui.label.block_snap", shape.getBlockSnap().getDisplayName());
        int width = this.getStringWidth(label) + 10;
        ConfigButtonOptionList buttonSnap = new ConfigButtonOptionList(x + button.getWidth() + 4, y, width, 20, this.configBlockSnap, label);
        this.addButton(buttonSnap, new ButtonListenerSphereBlockSnap(shape, this));

        y += 34;

        this.createColorInput(x, y);
    }

    private void createShapeEditorElementDoubleField(int x, int y, DoubleSupplier supplier, DoubleConsumer consumer, String translationKey, boolean addButton)
    {
        this.addLabel(x + 12, y, -1, 12, 0xFFFFFFFF, translationKey);
        y += 11;

        GuiTextFieldDouble txtField = new GuiTextFieldDouble(x + 12, y, 40, 14, this.textRenderer);
        txtField.setText(String.valueOf(supplier.getAsDouble()));
        this.addTextField(txtField, new TextFieldListenerDouble(consumer));

        if (addButton)
        {
            String hover = StringUtils.translate("malilib.gui.button.hover.plus_minus_tip");
            ButtonGeneric button = new ButtonGeneric(x + 54, y - 1, MaLiLibIcons.BTN_PLUSMINUS_16, hover);
            this.addButton(button, new ButtonListenerDoubleModifier(supplier, new ChainedDoubleConsumer(consumer, (val) -> txtField.setText(String.valueOf(supplier.getAsDouble())) )));
        }
    }

    private void createShapeEditorElementIntField(int x, int y, IntSupplier supplier, IntConsumer consumer, String translationKey, boolean addButton)
    {
        this.addLabel(x + 12, y, -1, 12, 0xFFFFFFFF, translationKey);
        y += 11;

        GuiTextFieldInteger txtField = new GuiTextFieldInteger(x + 12, y, 40, 14, this.textRenderer);
        txtField.setText(String.valueOf(supplier.getAsInt()));
        this.addTextField(txtField, new TextFieldListenerInteger(consumer));

        if (addButton)
        {
            String hover = StringUtils.translate("malilib.gui.button.hover.plus_minus_tip");
            ButtonGeneric button = new ButtonGeneric(x + 54, y - 1, MaLiLibIcons.BTN_PLUSMINUS_16, hover);
            this.addButton(button, new ButtonListenerIntModifier(supplier, new ChainedIntConsumer(consumer, (val) -> txtField.setText(String.valueOf(supplier.getAsInt())) )));
        }
    }

    private void createDirectionButton(int x, int y, Supplier<Direction> supplier, Consumer<Direction> consumer, String translationKey)
    {
        this.addLabel(x, y, -1, 12, 0xFFFFFFFF, translationKey);
        y += 10;

        ButtonGeneric button = new ButtonGeneric(x, y, 50, 20, org.apache.commons.lang3.StringUtils.capitalize(supplier.get().toString().toLowerCase()));
        this.addButton(button, (btn, mouseBtn) -> { consumer.accept(cycleDirection(supplier.get(), mouseBtn == 1)); this.initGui(); } );
    }

    private void createRenderTypeButton(int x, int y, Supplier<ShapeRenderType> supplier, Consumer<ShapeRenderType> consumer, String translationKey)
    {
        this.addLabel(x, y, -1, 12, 0xFFFFFFFF, translationKey);
        y += 10;

        ButtonGeneric button = new ButtonGeneric(x, y, -1, 20, supplier.get().getDisplayName());
        this.addButton(button, (btn, mouseBtn) -> { consumer.accept((ShapeRenderType) supplier.get().cycle(mouseBtn == 0)); this.initGui(); } );
    }

    public static Direction cycleDirection(Direction direction, boolean reverse)
    {
        int index = direction.getId();

        if (reverse)
        {
            index = index == 0 ? 5 : index - 1;
        }
        else
        {
            index = index >= 5 ? 0 : index + 1;
        }

        return Direction.byId(index);
    }

    private record SphereEditor(ShapeCircleBase shape, GuiShapeEditor gui) implements ICoordinateValueModifier
    {

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

    private record ButtonListenerSphere(ShapeCircleBase shape, GuiShapeEditor gui) implements IButtonActionListener
    {

        @Override
        public void actionPerformedWithButton(ButtonBase button, int mouseButton)
        {
            Entity entity = this.gui.mc.getCameraEntity();

            if (entity != null)
            {
                this.shape.setCenter(entity.getPos());
                this.gui.initGui();
            }
        }
    }

    private record ButtonListenerSphereBlockSnap(ShapeCircleBase shape,
                                                 GuiShapeEditor gui) implements IButtonActionListener
    {

        @Override
        public void actionPerformedWithButton(ButtonBase button, int mouseButton)
        {
            this.shape.setBlockSnap((BlockSnap) this.gui.configBlockSnap.getOptionListValue());
            this.gui.initGui();
        }
    }

    private record TextFieldListenerColor(ShapeBase shape) implements ITextFieldListener<GuiTextFieldGeneric>
    {

        @Override
        public boolean onTextChange(GuiTextFieldGeneric textField)
        {
            this.shape.setColorFromString(textField.getText());
            return false;
        }
    }

    private record TextFieldListenerInteger(IntConsumer consumer) implements ITextFieldListener<GuiTextFieldInteger>
    {

        @Override
        public boolean onTextChange(GuiTextFieldInteger textField)
        {
            try
            {
                this.consumer.accept(Integer.parseInt(textField.getText()));
                return true;
            }
            catch (Exception ignore) {}

            return false;
        }
    }

    private record TextFieldListenerDouble(DoubleConsumer consumer) implements ITextFieldListener<GuiTextFieldDouble>
    {

        @Override
        public boolean onTextChange(GuiTextFieldDouble textField)
        {
            try
            {
                this.consumer.accept(Double.parseDouble(textField.getText()));
                return true;
            }
            catch (Exception ignore) {}

            return false;
        }
    }

    private record ChainedDoubleConsumer(DoubleConsumer consumerOne,
                                         DoubleConsumer consumerTwo) implements DoubleConsumer
    {

        @Override
        public void accept(double value)
        {
            this.consumerOne.accept(value);
            this.consumerTwo.accept(value);
        }
    }

    private record ChainedIntConsumer(IntConsumer consumerOne,
                                      IntConsumer consumerTwo) implements IntConsumer
    {

        @Override
        public void accept(int value)
        {
            this.consumerOne.accept(value);
            this.consumerTwo.accept(value);
        }
    }

    public static class ButtonListenerIntModifier implements IButtonActionListener
    {
        protected final IntSupplier supplier;
        protected final IntConsumer consumer;
        protected final int modifierShift;
        protected final int modifierControl;
        protected final int modifierAlt;

        public ButtonListenerIntModifier(IntSupplier supplier, IntConsumer consumer)
        {
            this(supplier, consumer, 8, 1, 4);
        }

        public ButtonListenerIntModifier(IntSupplier supplier, IntConsumer consumer, int modifierShift, int modifierControl, int modifierAlt)
        {
            this.supplier = supplier;
            this.consumer = consumer;
            this.modifierShift = modifierShift;
            this.modifierControl = modifierControl;
            this.modifierAlt = modifierAlt;
        }

        @Override
        public void actionPerformedWithButton(ButtonBase button, int mouseButton)
        {
            int amount = mouseButton == 1 ? -1 : 1;

            if (GuiBase.isShiftDown()) { amount *= this.modifierShift; }
            if (GuiBase.isCtrlDown())  { amount *= this.modifierControl; }
            if (GuiBase.isAltDown())   { amount *= this.modifierAlt; }

            this.consumer.accept(this.supplier.getAsInt() + amount);
        }
    }

    public static class ButtonListenerDoubleModifier implements IButtonActionListener
    {
        protected final DoubleSupplier supplier;
        protected final DoubleConsumer consumer;
        protected final int modifierShift;
        protected final int modifierControl;
        protected final int modifierAlt;

        public ButtonListenerDoubleModifier(DoubleSupplier supplier, DoubleConsumer consumer)
        {
            this(supplier, consumer, 8, 1, 4);
        }

        public ButtonListenerDoubleModifier(DoubleSupplier supplier, DoubleConsumer consumer, int modifierShift, int modifierControl, int modifierAlt)
        {
            this.supplier = supplier;
            this.consumer = consumer;
            this.modifierShift = modifierShift;
            this.modifierControl = modifierControl;
            this.modifierAlt = modifierAlt;
        }

        @Override
        public void actionPerformedWithButton(ButtonBase button, int mouseButton)
        {
            int amount = mouseButton == 1 ? -1 : 1;

            if (GuiBase.isShiftDown()) { amount *= this.modifierShift; }
            if (GuiBase.isCtrlDown())  { amount *= this.modifierControl; }
            if (GuiBase.isAltDown())   { amount *= this.modifierAlt; }

            this.consumer.accept(this.supplier.getAsDouble() + amount);
        }
    }

    public static class WidgetColorIndicator extends WidgetBase
    {
        protected final IConfigInteger config;

        public WidgetColorIndicator(int x, int y, int width, int height, Color4f color, IntConsumer consumer)
        {
            this(x, y, width, height, new ConfigInteger("", color.intValue, ""));

            ((ConfigInteger) this.config).setValueChangeCallback((cfg) -> consumer.accept(cfg.getIntegerValue()) );
        }

        public WidgetColorIndicator(int x, int y, int width, int height, IConfigInteger config)
        {
            super(x, y, width, height);

            this.config = config;
            //this.addHoverString(StringUtils.translate("malilib.gui.hover.open_color_editor"));
        }

        @Override
        protected boolean onMouseClickedImpl(int mouseX, int mouseY, int mouseButton)
        {
            GuiColorEditorHSV gui = new GuiColorEditorHSV(this.config, null, GuiUtils.getCurrentScreen());
            GuiBase.openGui(gui);
            return true;
        }

        @Override
        public void postRenderHovered(int mouseX, int mouseY, boolean selected, MatrixStack matrixStack)
        {
            RenderUtils.drawHoverText(mouseX, mouseY, ImmutableList.of("Open Color Editor"), matrixStack);
        }

        @Override
        public void render(int mouseX, int mouseY, boolean selected, MatrixStack matrixStack)
        {
            int x = this.getX();
            int y = this.getY();
            int z = this.zLevel;
            int width = this.getWidth();
            int height = this.getHeight();

            RenderUtils.drawRect(x    , y + 0, width    , height    , 0xFFFFFFFF, z);
            RenderUtils.drawRect(x + 1, y + 1, width - 2, height - 2, 0xFF000000, z);
            RenderUtils.drawRect(x + 2, y + 2, width - 4, height - 4, 0xFF000000 | this.config.getIntegerValue(), z);
        }
    }
}
