package fi.dy.masa.minihud.gui;

import fi.dy.masa.malilib.config.options.ConfigOptionList;
import fi.dy.masa.malilib.gui.GuiRenderLayerEditBase;
import fi.dy.masa.malilib.gui.GuiTextFieldDouble;
import fi.dy.masa.malilib.gui.GuiTextFieldGeneric;
import fi.dy.masa.malilib.gui.GuiTextFieldInteger;
import fi.dy.masa.malilib.gui.button.ButtonBase;
import fi.dy.masa.malilib.gui.button.ButtonGeneric;
import fi.dy.masa.malilib.gui.button.ConfigButtonOptionList;
import fi.dy.masa.malilib.gui.button.IButtonActionListener;
import fi.dy.masa.malilib.gui.interfaces.ITextFieldListener;
import fi.dy.masa.malilib.interfaces.ICoordinateValueModifier;
import fi.dy.masa.malilib.render.RenderUtils;
import fi.dy.masa.malilib.util.BlockSnap;
import fi.dy.masa.malilib.util.GuiUtils;
import fi.dy.masa.malilib.util.LayerRange;
import fi.dy.masa.malilib.util.PositionUtils;
import fi.dy.masa.malilib.util.PositionUtils.CoordinateType;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.renderer.shapes.ShapeBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeDespawnSphere;
import fi.dy.masa.minihud.renderer.shapes.ShapeSphere;
import net.minecraft.entity.player.PlayerEntity;

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
        int y = 26;

        this.createLayerEditControls(x, y, this.getLayerRange());
        this.createShapeEditorElements(x, this.nextY);
        this.createColorInput(22, this.nextY);
    }

    @Override
    protected LayerRange getLayerRange()
    {
        return this.shape.getLayerRange();
    }

    @Override
    protected void drawContents(int mouseX, int mouseY, float partialTicks)
    {
        int x = 96;
        int y = this.colorY;
        RenderUtils.drawRect(x    , y + 0, 19, 19, 0xFFFFFFFF);
        RenderUtils.drawRect(x + 1, y + 1, 17, 17, 0xFF000000);
        RenderUtils.drawRect(x + 2, y + 2, 15, 15, 0xFF000000 | this.shape.getColor().intValue);
    }

    private void createColorInput(int x, int y)
    {
        String label = StringUtils.translate("minihud.gui.label.color");
        int w = this.getStringWidth(label);
        this.addLabel(x, y, w, 14, 0xFFFFFFFF, label);
        y += 12;

        GuiTextFieldGeneric textField = new GuiTextFieldGeneric(x, y, 70, 17, this.textRenderer);
        textField.setMaxLength(12);
        textField.setText(String.format("#%08X", this.shape.getColor().intValue));
        this.addTextField(textField, new TextFieldListenerColor(this.shape));
        this.nextY = y + 20;
        this.colorY = y - 1;
    }

    private void createShapeEditorElements(int x, int y)
    {
        switch (this.shape.getType())
        {
            case DESPAWN_SPHERE:
                this.createShapeEditorElementsDespawnSphere(x, y);
                break;
            case SPHERE:
                this.createShapeEditorElementsSphere(x, y);
                break;
        }
    }

    private void createShapeEditorElementsDespawnSphere(int x, int y)
    {
        this.addLabel(x, y, 60, 14, 0xFFFFFFFF, StringUtils.translate("minihud.gui.label.center_colon"));
        y += 12;
        ShapeDespawnSphere shape = (ShapeDespawnSphere) this.shape;
        GuiUtils.createVec3dInputsVertical(x, y, 120, shape.getCenter(), new DespawnSphereEditor(shape, this), true, this);
        y += 54;

        ButtonGeneric button = new ButtonGeneric(x + 11, y, -1, false, "malilib.gui.button.render_layers_gui.set_here");
        this.addButton(button, new ButtonListenerDespawnSphere(shape, this));
        y += 30;

        String label = StringUtils.translate("minihud.gui.label.margin_colon");
        int w = this.getStringWidth(label);
        this.addLabel(x + 12, y, w, 12, 0xFFFFFFFF, label);
        y += 12;

        GuiTextFieldDouble txtField = new GuiTextFieldDouble(x + 12, y, 60, 16, this.textRenderer);
        txtField.setText(String.valueOf(shape.getMargin()));
        this.addTextField(txtField, new TextFieldListenerMarginDespawnSphere(shape));
        y += 20;

        // Color input in this spot
        this.nextY = y;

        y += 36;
        w = this.getSnapButtonWidth();
        this.configBlockSnap.setOptionListValue(shape.getBlockSnap());
        ConfigButtonOptionList buttonSnap = new ConfigButtonOptionList(x + 11, y, w, 20, this.configBlockSnap, "minihud.gui.label.block_snap");
        ButtonListenerDespawnSphereBlockSnap listener = new ButtonListenerDespawnSphereBlockSnap(shape, this);
        this.addButton(buttonSnap, listener);
        y += 22;
    }

    private int getSnapButtonWidth()
    {
        int width = 0;

        for (BlockSnap val : BlockSnap.values())
        {
            width = Math.max(width, this.getStringWidth(StringUtils.translate("minihud.gui.label.block_snap", val.getDisplayName())) + 10);
        }

        return width;
    }

    private static class DespawnSphereEditor implements ICoordinateValueModifier
    {
        private final GuiShapeEditor gui;
        private final ShapeDespawnSphere shape;

        private DespawnSphereEditor(ShapeDespawnSphere shape, GuiShapeEditor gui)
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

    private static class ButtonListenerDespawnSphere implements IButtonActionListener
    {
        private final GuiShapeEditor gui;
        private final ShapeDespawnSphere shape;

        private ButtonListenerDespawnSphere(ShapeDespawnSphere shape, GuiShapeEditor gui)
        {
            this.shape = shape;
            this.gui = gui;
        }

        @Override
        public void actionPerformedWithButton(ButtonBase button, int mouseButton)
        {
            PlayerEntity player = this.gui.mc.player;

            if (player != null)
            {
                this.shape.setCenter(player.getPos());
                this.gui.initGui();
            }
        }
    }

    private static class ButtonListenerDespawnSphereBlockSnap implements IButtonActionListener
    {
        private final GuiShapeEditor gui;
        private final ShapeDespawnSphere shape;

        private ButtonListenerDespawnSphereBlockSnap(ShapeDespawnSphere shape, GuiShapeEditor gui)
        {
            this.shape = shape;
            this.gui = gui;
        }

        @Override
        public void actionPerformedWithButton(ButtonBase button, int mouseButton)
        {
            this.shape.setBlockSnap((BlockSnap) this.gui.configBlockSnap.getOptionListValue());
        }
    }

    private static class TextFieldListenerColor implements ITextFieldListener<GuiTextFieldGeneric>
    {
        private final ShapeBase shape;

        private TextFieldListenerColor(ShapeBase shape)
        {
            this.shape = shape;
        }

        @Override
        public boolean onTextChange(GuiTextFieldGeneric textField)
        {
            this.shape.setColorFromString(textField.getText());
            return false;
        }
    }

    private static class TextFieldListenerMarginDespawnSphere implements ITextFieldListener<GuiTextFieldDouble>
    {
        private final ShapeDespawnSphere shape;

        private TextFieldListenerMarginDespawnSphere(ShapeDespawnSphere shape)
        {
            this.shape = shape;
        }

        @Override
        public boolean onTextChange(GuiTextFieldDouble textField)
        {
            try
            {
                this.shape.setMargin(Double.parseDouble(textField.getText()));
                return true;
            }
            catch (Exception e) {}

            return false;
        }
    }

    private void createShapeEditorElementsSphere(int x, int y)
    {
        this.addLabel(x, y, 60, 14, 0xFFFFFFFF, StringUtils.translate("minihud.gui.label.center_colon"));
        y += 12;
        ShapeSphere shape = (ShapeSphere) this.shape;
        GuiUtils.createVec3dInputsVertical(x, y, 120, shape.getCenter(), new SphereEditor(shape, this), true, this);
        y += 54;

        ButtonGeneric button = new ButtonGeneric(x + 11, y, -1, false, "malilib.gui.button.render_layers_gui.set_here");
        this.addButton(button, new ButtonListenerSphere(shape, this));
        y += 30;

        //Margin
        String label = StringUtils.translate("minihud.gui.label.margin_colon");
        int w = this.getStringWidth(label);
        this.addLabel(x + 12, y, w, 12, 0xFFFFFFFF, label);
        y += 12;

        GuiTextFieldDouble txtField = new GuiTextFieldDouble(x + 12, y, 60, 16, this.textRenderer);
        txtField.setText(String.valueOf(shape.getMargin()));
        this.addTextField(txtField, new TextFieldListenerMarginSphere(shape));
        y += 20;

        // Radius
        String labelRadius = StringUtils.translate("minihud.gui.label.radius_colon");
        w = this.getStringWidth(labelRadius);
        this.addLabel(x + 12, y, w, 12, 0xFFFFFFFF, labelRadius);
        y += 12;

        GuiTextFieldInteger txtFieldRadius = new GuiTextFieldInteger(x + 12, y, 60, 16, this.textRenderer);
        txtFieldRadius.setText(String.valueOf(shape.getRadius()));
        this.addTextField(txtFieldRadius, new TextFieldListenerRadiusSphere(shape));
        y += 20;

        //Color input in this spot
        this.nextY = y;
        y += 36;

        w = this.getSnapButtonWidth();
        this.configBlockSnap.setOptionListValue(shape.getBlockSnap());
        ConfigButtonOptionList buttonSnap = new ConfigButtonOptionList(x + 11, y, w, 20, this.configBlockSnap, "minihud.gui.label.block_snap");
        ButtonListenerSphereBlockSnap listener = new ButtonListenerSphereBlockSnap(shape, this);
        this.addButton(buttonSnap, listener);
        y += 22;
    }

    private static class SphereEditor implements ICoordinateValueModifier
    {
        private final GuiShapeEditor gui;
        private final ShapeSphere shape;

        private SphereEditor(ShapeSphere shape, GuiShapeEditor gui)
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

    private static class ButtonListenerSphere implements IButtonActionListener
    {
        private final GuiShapeEditor gui;
        private final ShapeSphere shape;

        private ButtonListenerSphere(ShapeSphere shape, GuiShapeEditor gui)
        {
            this.shape = shape;
            this.gui = gui;
        }

        @Override
        public void actionPerformedWithButton(ButtonBase button, int mouseButton)
        {
            PlayerEntity player = this.gui.mc.player;

            if (player != null)
            {
                this.shape.setCenter(player.getPos());
                this.gui.initGui();
            }
        }
    }

    private static class ButtonListenerSphereBlockSnap implements IButtonActionListener
    {
        private final GuiShapeEditor gui;
        private final ShapeSphere shape;

        private ButtonListenerSphereBlockSnap(ShapeSphere shape, GuiShapeEditor gui)
        {
            this.shape = shape;
            this.gui = gui;
        }

        @Override
        public void actionPerformedWithButton(ButtonBase button, int mouseButton)
        {
            this.shape.setBlockSnap((BlockSnap) this.gui.configBlockSnap.getOptionListValue());
        }
    }


    private static class TextFieldListenerMarginSphere implements ITextFieldListener<GuiTextFieldDouble>
    {
        private final ShapeSphere shape;

        private TextFieldListenerMarginSphere(ShapeSphere shape)
        {
            this.shape = shape;
        }

        @Override
        public boolean onTextChange(GuiTextFieldDouble textField)
        {
            try
            {
                this.shape.setMargin(Double.parseDouble(textField.getText()));
                return true;
            }
            catch (Exception e) {}

            return false;
        }
    }

    private static class TextFieldListenerRadiusSphere implements ITextFieldListener<GuiTextFieldInteger>
    {
        private final ShapeSphere shape;

        private TextFieldListenerRadiusSphere(ShapeSphere shape)
        {
            this.shape = shape;
        }

        @Override
        public boolean onTextChange(GuiTextFieldInteger textField)
        {
            try
            {
                this.shape.setRadius(Integer.parseInt(textField.getText()));
                return true;
            }
            catch (Exception e) {}

            return false;
        }
    }
}
