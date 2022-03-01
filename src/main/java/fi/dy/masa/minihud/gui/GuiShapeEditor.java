package fi.dy.masa.minihud.gui;

import java.util.Locale;
import java.util.function.Consumer;
import java.util.function.DoubleConsumer;
import java.util.function.DoubleSupplier;
import java.util.function.IntConsumer;
import java.util.function.IntSupplier;
import java.util.function.Supplier;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.font.TextRenderer;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Box;
import net.minecraft.util.math.Direction;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.config.options.ConfigOptionList;
import fi.dy.masa.malilib.gui.GuiBase;
import fi.dy.masa.malilib.gui.GuiRenderLayerEditBase;
import fi.dy.masa.malilib.gui.GuiTextFieldDouble;
import fi.dy.masa.malilib.gui.GuiTextFieldGeneric;
import fi.dy.masa.malilib.gui.GuiTextFieldInteger;
import fi.dy.masa.malilib.gui.MaLiLibIcons;
import fi.dy.masa.malilib.gui.button.ButtonBase;
import fi.dy.masa.malilib.gui.button.ButtonGeneric;
import fi.dy.masa.malilib.gui.button.ButtonOnOff;
import fi.dy.masa.malilib.gui.button.ConfigButtonOptionList;
import fi.dy.masa.malilib.gui.button.IButtonActionListener;
import fi.dy.masa.malilib.gui.interfaces.ITextFieldListener;
import fi.dy.masa.malilib.gui.widgets.WidgetCheckBox;
import fi.dy.masa.malilib.gui.widgets.WidgetColorIndicator;
import fi.dy.masa.malilib.interfaces.ICoordinateValueModifier;
import fi.dy.masa.malilib.util.BlockSnap;
import fi.dy.masa.malilib.util.EntityUtils;
import fi.dy.masa.malilib.util.GuiUtils;
import fi.dy.masa.malilib.util.LayerRange;
import fi.dy.masa.malilib.util.PositionUtils;
import fi.dy.masa.malilib.util.PositionUtils.CoordinateType;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.gui.GuiConfigs.ConfigGuiTab;
import fi.dy.masa.minihud.renderer.shapes.ShapeBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeBlocky;
import fi.dy.masa.minihud.renderer.shapes.ShapeBox;
import fi.dy.masa.minihud.renderer.shapes.ShapeCircle;
import fi.dy.masa.minihud.renderer.shapes.ShapeCircleBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeLineBlock;
import fi.dy.masa.minihud.renderer.shapes.ShapeSpawnSphere;
import fi.dy.masa.minihud.renderer.shapes.ShapeType;
import fi.dy.masa.minihud.util.ShapeRenderType;

public class GuiShapeEditor extends GuiRenderLayerEditBase
{
    private final ShapeBase shape;
    private ConfigOptionList configBlockSnap;
    private int colorY;

    public GuiShapeEditor(ShapeBase shape)
    {
        this.shape = shape;
        this.title = StringUtils.translate("minihud.gui.title.shape_editor", shape.getDisplayName());
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
        ShapeType type = this.shape.getType();

        switch (type)
        {
            case BOX:
                this.createShapeEditorElementsBox(x, y);
                break;

            case BLOCK_LINE:
                this.createShapeEditorElementsBlockLine(x, y);
                break;

            case ADJUSTABLE_SPAWN_SPHERE:
            case CAN_DESPAWN_SPHERE:
            case CAN_SPAWN_SPHERE:
            case DESPAWN_SPHERE:
            {
                ShapeSpawnSphere shape = (ShapeSpawnSphere) this.shape;
                boolean isAdjustable = type == ShapeType.ADJUSTABLE_SPAWN_SPHERE;
                this.createShapeEditorElementsSphereBase(x, y, isAdjustable);

                if (isAdjustable == false)
                {
                    this.createShapeEditorElementDoubleField(x + 150, y + 2, shape::getMargin, shape::setMargin, "minihud.gui.label.margin_colon", false);
                }

                if (shape instanceof ShapeSpawnSphere)
                {
                    String key = "minihud.gui.button.shape_renderer.spawn_sphere.toggle_use_quadrants";
                    String hover = StringUtils.translate("minihud.gui.button.hover.shape_renderer.spawn_sphere.toggle_use_quadrants");
                    ButtonOnOff button = new ButtonOnOff(x + 160, y + 30, -1, false, key, shape.getUseCornerQuadrants(), hover);
                    this.addButton(button, (btn, mbtn) -> this.toggleUseQuadrants(shape, button));
                }

                this.createLayerEditControls(146, 162, this.getLayerRange());
                break;
            }

            case CIRCLE:
            {
                ShapeCircle shape = (ShapeCircle) this.shape;
                this.createShapeEditorElementsSphereBase(x, y, true);
                this.createShapeEditorElementIntField(x + 150, y + 36, shape::getHeight, shape::setHeight, "minihud.gui.label.height_colon", true);
                this.createDirectionButton(x + 230, y + 36, shape::getMainAxis, shape::setMainAxis, "minihud.gui.label.shape.circle.main_axis_colon");
                this.createRenderTypeButton(renderTypeX, renderTypeY, this.shape::getRenderType, this.shape::setRenderType, "minihud.gui.label.shape.render_type_colon");
                this.createLayerEditControls(146, 162, this.getLayerRange());
                break;
            }

            case SPHERE_BLOCKY:
                this.createShapeEditorElementsSphereBase(x, y, true);
                this.createRenderTypeButton(renderTypeX, renderTypeY, this.shape::getRenderType, this.shape::setRenderType, "minihud.gui.label.shape.render_type_colon");
                this.createLayerEditControls(146, 162, this.getLayerRange());
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
        GuiUtils.createVec3dInputsVertical(x, y, 120, shape.getEffectiveCenter(), new Vec3dEditor(shape::getEffectiveCenter, shape::setCenter, this), true, this);
        x += 11;
        y += 54;

        int btnX = x;
        ButtonGeneric button = new ButtonGeneric(btnX, y, -1, false, "malilib.gui.button.render_layers_gui.set_to_player");
        this.addButton(button, (btn, mbtn) -> this.setPositionFromCamera(shape::setCenter));
        btnX = button.getX() + button.getWidth() + 4;

        this.configBlockSnap.setOptionListValue(shape.getBlockSnap());
        String label = StringUtils.translate("minihud.gui.label.shape.block_snap", shape.getBlockSnap().getDisplayName());
        int width = this.getStringWidth(label) + 10;

        ConfigButtonOptionList buttonSnap = new ConfigButtonOptionList(btnX, y, width, 20, this.configBlockSnap, label);
        this.addButton(buttonSnap, new ButtonListenerSphereBlockSnap(shape, this));
        btnX = buttonSnap.getX() + buttonSnap.getWidth() + 4;

        ButtonOnOff combineQuadsButton = new ButtonOnOff(btnX, y, -1, false, "minihud.gui.button.shape_renderer.toggle_combine_quads", ((ShapeBlocky) this.shape).getCombineQuads());
        this.addButton(combineQuadsButton, (b, mb) -> this.toggleCombineQuads(shape, combineQuadsButton));
        y += 34;

        this.createColorInput(x, y);
    }

    private void createShapeEditorElementsBox(int xIn, int yIn)
    {
        ShapeBox shape = (ShapeBox) this.shape;

        int x = xIn;
        int y = yIn + 4;

        this.createBoxInputs(x, y, x, y + 82, 120, shape::getBox, shape::setBox);

        y += 160;
        this.createColorInput(x, y);

        x = xIn + 250;
        y = yIn + 4;
        this.addBoxSideToggleCheckbox(x, y     , Direction.DOWN,  shape);
        this.addBoxSideToggleCheckbox(x, y + 11, Direction.UP,    shape);
        this.addBoxSideToggleCheckbox(x, y + 22, Direction.NORTH, shape);
        this.addBoxSideToggleCheckbox(x, y + 33, Direction.SOUTH, shape);
        this.addBoxSideToggleCheckbox(x, y + 44, Direction.WEST,  shape);
        this.addBoxSideToggleCheckbox(x, y + 55, Direction.EAST,  shape);

        x = xIn + 160;
        y = yIn + 4;

        if (shape.isGridEnabled())
        {
            this.addLabel(x, y, 60, 14, 0xFFFFFFFF, StringUtils.translate("minihud.gui.label.shape.box.grid_size"));
            GuiUtils.createVec3dInputsVertical(x, y + 12, 50, shape.getGridSize(),
                                               new Vec3dEditor(shape::getGridSize, shape::setGridSize, this), true, this);

            y += 70;
            this.addLabel(x, y, 60, 14, 0xFFFFFFFF, StringUtils.translate("minihud.gui.label.shape.box.grid_start_offset"));
            GuiUtils.createVec3dInputsVertical(x, y + 12, 50, shape.getGridStartOffset(),
                                               new Vec3dEditor(shape::getGridStartOffset, shape::setGridStartOffset, this), true, this);

            this.addLabel(x + 100, y, 60, 14, 0xFFFFFFFF, StringUtils.translate("minihud.gui.label.shape.box.grid_end_offset"));
            GuiUtils.createVec3dInputsVertical(x + 100, y + 12, 50, shape.getGridEndOffset(),
                                               new Vec3dEditor(shape::getGridEndOffset, shape::setGridEndOffset, this), true, this);
        }

        y = yIn + 148;
        ButtonGeneric button = new ButtonOnOff(x, y, -1, false, "minihud.gui.label.shape.box.grid_enabled", shape.isGridEnabled());
        this.addButton(button, (btn, mbtn) -> this.toggleGridEnabled(shape));
    }

    private void createShapeEditorElementsBlockLine(int xIn, int yIn)
    {
        ShapeLineBlock shape = (ShapeLineBlock) this.shape;

        int x = xIn;
        int x2 = x + 160;
        int y = yIn + 4;

        this.addLabel(x, y, -1, 14, 0xFFFFFFFF, StringUtils.translate("minihud.gui.label.shape.box.minimum_coord"));
        this.addLabel(x2, y, -1, 14, 0xFFFFFFFF, StringUtils.translate("minihud.gui.label.shape.box.maximum_coord"));
        y += 14;

        GuiUtils.createVec3dInputsVertical(x , y, 120, shape.getStartPos(), new Vec3dEditor(shape::getStartPos, shape::setStartPos, this), true, this);
        GuiUtils.createVec3dInputsVertical(x2, y, 120, shape.getEndPos(), new Vec3dEditor(shape::getEndPos, shape::setEndPos, this), true, this);
        y += 54;

        ButtonGeneric btn = new ButtonGeneric(x + 11, y, -1, 20, StringUtils.translate("malilib.gui.button.render_layers_gui.set_to_player"));
        this.addButton(btn, (b, mb) -> this.setPositionFromCamera(shape::setStartPos));

        btn = new ButtonGeneric(x2 + 11, y, -1, 20, StringUtils.translate("malilib.gui.button.render_layers_gui.set_to_player"));
        this.addButton(btn, (b, mb) -> this.setPositionFromCamera(shape::setEndPos));
        y += 24;

        int btnX = xIn + 11;
        this.configBlockSnap.setOptionListValue(shape.getBlockSnap());
        String label = StringUtils.translate("minihud.gui.label.shape.block_snap", shape.getBlockSnap().getDisplayName());
        int width = this.getStringWidth(label) + 10;

        ConfigButtonOptionList buttonSnap = new ConfigButtonOptionList(btnX, y, width, 20, this.configBlockSnap, label);
        this.addButton(buttonSnap, new ButtonListenerSphereBlockSnap(shape, this));
        btnX += buttonSnap.getWidth() + 4;

        ButtonOnOff combineQuadsButton = new ButtonOnOff(btnX, y, -1, false, "minihud.gui.button.shape_renderer.toggle_combine_quads", ((ShapeBlocky) this.shape).getCombineQuads());
        this.addButton(combineQuadsButton, (b, mb) -> this.toggleCombineQuads(shape, combineQuadsButton));
        y += 24;

        this.createColorInput(xIn + 12, y);
        y += 11;

        this.createLayerEditControls(xIn + 115, y, this.getLayerRange());
    }

    private void toggleGridEnabled(ShapeBox shape)
    {
        shape.toggleGridEnabled();
        this.initGui();
    }

    private void toggleCombineQuads(ShapeBlocky shape, ButtonOnOff button)
    {
        shape.toggleCombineQuads();
        button.updateDisplayString(shape.getCombineQuads());
    }

    private void toggleUseQuadrants(ShapeSpawnSphere shape, ButtonOnOff button)
    {
        shape.toggleUseCornerQuadrants();
        button.updateDisplayString(shape.getUseCornerQuadrants());
    }

    private void addBoxSideToggleCheckbox(int x, int y, Direction side, ShapeBox shape)
    {
        WidgetCheckBox cb = new WidgetCheckBox(x, y, MaLiLibIcons.MINUS, MaLiLibIcons.PLUS, this.capitalize(side.getName()), "Render the " + side.getName() + " side of the box");
        cb.setChecked(shape.isSideEnabled(side));
        cb.setListener((w) -> this.toggleSideEnabled(side, shape));
        this.addWidget(cb);
    }

    private void toggleSideEnabled(Direction side, ShapeBox shape)
    {
        int mask = shape.getEnabledSidesMask();
        shape.setEnabledSidesMask(mask ^ (1 << side.getId()));
    }


    public void createBoxInputs(int x1, int y1, int x2, int y2, int textFieldWidth,
                                Supplier<Box> supplier, Consumer<Box> consumer)
    {
        this.addLabel(x1, y1, -1, 14, 0xFFFFFFFF, StringUtils.translate("minihud.gui.label.shape.box.minimum_coord"));
        y1 += 12;

        this.addLabel(x2, y2, -1, 14, 0xFFFFFFFF, StringUtils.translate("minihud.gui.label.shape.box.maximum_coord"));
        y2 += 12;

        int yInc = 16;
        this.addLabel(x1, y1           , CoordinateType.X);
        this.addLabel(x1, y1 + yInc    , CoordinateType.Y);
        this.addLabel(x1, y1 + yInc * 2, CoordinateType.Z);

        this.addLabel(x2, y2           , CoordinateType.X);
        this.addLabel(x2, y2 + yInc    , CoordinateType.Y);
        this.addLabel(x2, y2 + yInc * 2, CoordinateType.Z);

        MutableWrapperBox mutableBox = new MutableWrapperBox(supplier.get(), consumer);
        int x = x1 + 12;
        
        this.addBoxInput(x, y1            + 1, textFieldWidth, mutableBox::getMinX, mutableBox::setMinX);
        this.addBoxInput(x, y1 + yInc     + 1, textFieldWidth, mutableBox::getMinY, mutableBox::setMinY);
        this.addBoxInput(x, y1 + yInc * 2 + 1, textFieldWidth, mutableBox::getMinZ, mutableBox::setMinZ);

        ButtonGeneric btn = new ButtonGeneric(x, y1 + yInc * 3 + 2, -1, 14, StringUtils.translate("malilib.gui.button.render_layers_gui.set_to_player"));
        btn.setRenderDefaultBackground(false);
        this.addButton(btn, (b, mb) -> this.setPositionFromCamera(mutableBox::setMinCorner));

        x = x2 + 12;
        this.addBoxInput(x, y2            + 1, textFieldWidth, mutableBox::getMaxX, mutableBox::setMaxX);
        this.addBoxInput(x, y2 + yInc     + 1, textFieldWidth, mutableBox::getMaxY, mutableBox::setMaxY);
        this.addBoxInput(x, y2 + yInc * 2 + 1, textFieldWidth, mutableBox::getMaxZ, mutableBox::setMaxZ);

        btn = new ButtonGeneric(x, y2 + yInc * 3 + 2, -1, 14, StringUtils.translate("malilib.gui.button.render_layers_gui.set_to_player"));
        btn.setRenderDefaultBackground(false);
        this.addButton(btn, (b, mb) -> this.setPositionFromCamera(mutableBox::setMaxCorner));
    }

    protected void addBoxInput(int x, int y, int textFieldWidth, DoubleSupplier coordinateSource,
                               DoubleConsumer coordinateConsumer)
    {
        TextRenderer textRenderer = MinecraftClient.getInstance().textRenderer;
        GuiTextFieldGeneric textField = new GuiTextFieldGeneric(x, y + 1, textFieldWidth, 14, textRenderer);
        textField.setText("" + coordinateSource.getAsDouble());

        this.addTextFieldAndButtonForBoxCoordinate(x + textFieldWidth + 4, y, textField,
                                                   coordinateSource, coordinateConsumer);
    }

    protected int addLabel(int x, int y, CoordinateType type)
    {
        String label = type.name() + ":";
        int labelWidth = 12;
        this.addLabel(x, y, labelWidth, 20, 0xFFFFFFFF, label);
        return labelWidth;
    }

    protected void addTextFieldAndButtonForBoxCoordinate(int x, int y, GuiTextFieldGeneric textField,
                                                                DoubleSupplier coordinateSource,
                                                                DoubleConsumer coordinateConsumer)
    {
        this.addTextField(textField, new TextFieldListenerDouble(coordinateConsumer));

        String hover = StringUtils.translate("malilib.gui.button.hover.plus_minus_tip");
        ButtonGeneric button = new ButtonGeneric(x, y, MaLiLibIcons.BTN_PLUSMINUS_16, hover);
        this.addButton(button, new ButtonListenerDoubleModifier(coordinateSource, (v) -> {
            coordinateConsumer.accept(v);
            textField.setText("" + coordinateSource.getAsDouble());
        }));
    }

    private String capitalize(String str)
    {
        if (str.length() > 1)
        {
            return str.substring(0, 1).toUpperCase(Locale.ROOT) + str.substring(1);
        }

        return str.length() > 0 ? str.toUpperCase(Locale.ROOT) : str;
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

    protected void setPositionFromCamera(Consumer<Vec3d> consumer)
    {
        Entity entity = EntityUtils.getCameraEntity();

        if (entity != null)
        {
            consumer.accept(entity.getPos());
            this.initGui();
        }
    }

    protected void setBlockPosFromCamera(Consumer<BlockPos> consumer)
    {
        Entity entity = EntityUtils.getCameraEntity();

        if (entity != null)
        {
            consumer.accept(entity.getBlockPos());
            this.initGui();
        }
    }

    public static class MutableWrapperBox
    {
        protected final Consumer<Box> boxConsumer;
        protected double minX;
        protected double minY;
        protected double minZ;
        protected double maxX;
        protected double maxY;
        protected double maxZ;

        public MutableWrapperBox(Box box, Consumer<Box> boxConsumer)
        {
            this.minX = box.minX;
            this.minY = box.minY;
            this.minZ = box.minZ;
            this.maxX = box.maxX;
            this.maxY = box.maxY;
            this.maxZ = box.maxZ;
            this.boxConsumer = boxConsumer;
        }

        public double getMinX()
        {
            return this.minX;
        }

        public double getMinY()
        {
            return this.minY;
        }

        public double getMinZ()
        {
            return this.minZ;
        }

        public double getMaxX()
        {
            return this.maxX;
        }

        public double getMaxY()
        {
            return this.maxY;
        }

        public double getMaxZ()
        {
            return this.maxZ;
        }

        public void setMinX(double minX)
        {
            this.minX = minX;
            this.updateBox();
        }

        public void setMinY(double minY)
        {
            this.minY = minY;
            this.updateBox();
        }

        public void setMinZ(double minZ)
        {
            this.minZ = minZ;
            this.updateBox();
        }

        public void setMaxX(double maxX)
        {
            this.maxX = maxX;
            this.updateBox();
        }

        public void setMaxY(double maxY)
        {
            this.maxY = maxY;
            this.updateBox();
        }

        public void setMaxZ(double maxZ)
        {
            this.maxZ = maxZ;
            this.updateBox();
        }

        public void setMinCorner(Vec3d pos)
        {
            this.minX = pos.x;
            this.minY = pos.y;
            this.minZ = pos.z;
            this.updateBox();
        }

        public void setMaxCorner(Vec3d pos)
        {
            this.maxX = pos.x;
            this.maxY = pos.y;
            this.maxZ = pos.z;
            this.updateBox();
        }

        protected void updateBox()
        {
            Box box = new Box(this.minX, this.minY, this.minZ, this.maxX, this.maxY, this.maxZ);
            this.boxConsumer.accept(box);
        }
    }

    public record Vec3dEditor(Supplier<Vec3d> supplier, Consumer<Vec3d> consumer, GuiShapeEditor gui) implements ICoordinateValueModifier
    {
        @Override
        public boolean modifyValue(CoordinateType type, int amount)
        {
            this.consumer.accept(PositionUtils.modifyValue(type, this.supplier.get(), amount));
            this.gui.initGui();
            return true;
        }

        @Override
        public boolean setValueFromString(CoordinateType type, String newValue)
        {
            try
            {
                this.consumer.accept(PositionUtils.setValue(type, this.supplier.get(), Double.parseDouble(newValue)));
                return true;
            }
            catch (Exception ignore) {}

            return false;
        }
    }

    public record BlockPosEditor(Supplier<BlockPos> supplier, Consumer<BlockPos> consumer, GuiShapeEditor gui) implements ICoordinateValueModifier
    {
        @Override
        public boolean modifyValue(CoordinateType type, int amount)
        {
            this.consumer.accept(PositionUtils.modifyValue(type, this.supplier.get(), amount));
            this.gui.initGui();
            return true;
        }

        @Override
        public boolean setValueFromString(CoordinateType type, String newValue)
        {
            try
            {
                this.consumer.accept(PositionUtils.setValue(type, this.supplier.get(), Integer.parseInt(newValue)));
                return true;
            }
            catch (Exception ignore) {}

            return false;
        }
    }

    private record ButtonListenerSphereBlockSnap(ShapeBlocky shape, GuiShapeEditor gui) implements IButtonActionListener
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

    private record TextFieldListenerDouble(DoubleConsumer consumer) implements ITextFieldListener<GuiTextFieldGeneric>
    {
        @Override
        public boolean onTextChange(GuiTextFieldGeneric textField)
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

    private record ChainedDoubleConsumer(DoubleConsumer consumerOne, DoubleConsumer consumerTwo) implements DoubleConsumer
    {
        @Override
        public void accept(double value)
        {
            this.consumerOne.accept(value);
            this.consumerTwo.accept(value);
        }
    }

    private record ChainedIntConsumer(IntConsumer consumerOne, IntConsumer consumerTwo) implements IntConsumer
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
}
