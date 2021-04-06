package fi.dy.masa.minihud.gui;

import javax.annotation.Nullable;
import net.minecraft.client.gui.GuiScreen;
import fi.dy.masa.malilib.gui.BaseListScreen;
import fi.dy.masa.malilib.gui.widget.DropDownListWidget;
import fi.dy.masa.malilib.gui.widget.LabelWidget;
import fi.dy.masa.malilib.gui.widget.button.BooleanConfigButton;
import fi.dy.masa.malilib.gui.widget.button.GenericButton;
import fi.dy.masa.malilib.gui.widget.list.DataListWidget;
import fi.dy.masa.malilib.overlay.message.MessageUtils;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.gui.widget.WidgetShapeEntry;
import fi.dy.masa.minihud.renderer.shapes.ShapeBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager;
import fi.dy.masa.minihud.renderer.shapes.ShapeType;

public class GuiShapeManager extends BaseListScreen<DataListWidget<ShapeBase>>
{
    protected final DropDownListWidget<ShapeType> widgetDropDown;

    public GuiShapeManager()
    {
        super(10, 82, 20, 88, "minihud", ConfigScreen.ALL_TABS, ConfigScreen.SHAPES);

        this.title = StringUtils.translate("minihud.gui.title.shape_manager");

        // The position will get updated later
        this.widgetDropDown = new DropDownListWidget<>(0, 0, 160, 16, 200, 10, ShapeType.VALUES, ShapeType::getDisplayName, null);
        this.widgetDropDown.setZLevel((int) this.zLevel + 2);
    }

    @Override
    protected void initScreen()
    {
        super.initScreen();

        this.clearWidgets();
        this.clearButtons();
        this.createTabButtonContainerWidget();

        int x = 12;
        int y = 44;
        int lw = StringUtils.getMaxStringRenderWidth(StringUtils.translate("minihud.gui.button.shapes.main_rendering"),
                                                     StringUtils.translate("minihud.gui.button.shapes.shape_renderer")) + 8;

        LabelWidget label = new LabelWidget(x, y + 5, lw, -1, 0xFFFFF040, "minihud.gui.button.shapes.main_rendering");
        this.addWidget(label);

        x += label.getWidth() + 4;
        GenericButton button = new BooleanConfigButton(x, y, -1, 18, Configs.Generic.MAIN_RENDERING_TOGGLE);
        this.addWidget(button);

        this.widgetDropDown.setPosition(this.screenWidth - 10, y);
        this.widgetDropDown.setRightAlign(true, this.screenWidth - 10, true);
        this.addWidget(this.widgetDropDown);

        y += 18;
        x = 12;
        label = new LabelWidget(x, y + 6, lw, -1, 0xFFFFF040, "minihud.gui.button.shapes.shape_renderer");
        this.addWidget(label);

        x += label.getWidth() + 4;
        button = new BooleanConfigButton(x, y + 1, -1, 18, RendererToggle.SHAPE_RENDERER.getBooleanConfig());
        this.addWidget(button);

        button = new GenericButton(this.screenWidth - 10, y, -1, true, "minihud.gui.button.add_shape");
        this.addButton(button, (btn, mbtn) -> {
            ShapeType type = this.widgetDropDown.getSelectedEntry();

            if (type != null)
            {
                ShapeManager.INSTANCE.addShape(type.createShape());
                this.getListWidget().refreshEntries();
            }
            else
            {
                MessageUtils.error("minihud.message.error.shapes.select_shape_from_dropdown");
            }
        });
    }

    @Override
    public void onGuiClosed()
    {
        super.onGuiClosed();
    }

    public void onSelectionChange(@Nullable ShapeBase entry)
    {
        ShapeBase old = ShapeManager.INSTANCE.getSelectedShape();
        ShapeManager.INSTANCE.setSelectedShape(old == entry ? null : entry);
    }

    @Override
    protected DataListWidget<ShapeBase> createListWidget(int listX, int listY, int listWidth, int listHeight)
    {
        DataListWidget<ShapeBase> listWidget = new DataListWidget<>(listX, listY, listWidth, listHeight, ShapeManager.INSTANCE::getAllShapes);
        listWidget.setEntryWidgetFactory(WidgetShapeEntry::new);
        listWidget.setFetchFromSupplierOnRefresh(true);
        listWidget.setAllowSelection(true);
        listWidget.getEntrySelectionHandler().setSelectionListener(this::onSelectionChange);

        return listWidget;
    }

    public static boolean screenValidator(@Nullable GuiScreen currentScreen)
    {
        return currentScreen instanceof GuiShapeManager;
    }

    public static GuiShapeManager openShapeManager(@Nullable GuiScreen currentScreen)
    {
        GuiShapeManager gui = new GuiShapeManager();
        gui.setCurrentTab(ConfigScreen.SHAPES);
        return gui;
    }
}
