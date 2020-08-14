package fi.dy.masa.minihud.gui;

import javax.annotation.Nullable;
import org.lwjgl.input.Keyboard;
import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.gui.BaseListScreen;
import fi.dy.masa.malilib.gui.BaseScreen;
import fi.dy.masa.malilib.gui.widget.button.BooleanConfigButton;
import fi.dy.masa.malilib.gui.widget.button.GenericButton;
import fi.dy.masa.malilib.gui.config.ConfigTab;
import fi.dy.masa.malilib.gui.widget.DropDownListWidget;
import fi.dy.masa.malilib.gui.widget.LabelWidget;
import fi.dy.masa.malilib.gui.widget.list.DataListWidget;
import fi.dy.masa.malilib.gui.widget.list.entry.DataListEntrySelectionHandler;
import fi.dy.masa.malilib.render.message.MessageType;
import fi.dy.masa.malilib.render.message.MessageUtils;
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
        super(10, 68);

        this.title = StringUtils.translate("minihud.gui.title.shape_manager");

        // The position will get updated later
        this.widgetDropDown = new DropDownListWidget<>(0, 0, 160, 20, 200, 10, ImmutableList.copyOf(ShapeType.values()), ShapeType::getDisplayName);
        this.widgetDropDown.setZLevel((int) this.zLevel + 2);
    }

    @Override
    protected int getListWidth()
    {
        return this.width - 20;
    }

    @Override
    protected int getListHeight()
    {
        return this.height - this.getListY() - 6;
    }

    @Override
    public void initGui()
    {
        super.initGui();

        this.clearWidgets();
        this.clearButtons();
        this.createTabButtons();

        int x = this.width - 10;
        int y = 48;
        x -= this.addShapeAddButton(x, y);

        this.widgetDropDown.setPosition(x - this.widgetDropDown.getWidth() - 4, y);
        this.addWidget(this.widgetDropDown);

        x = 12;
        LabelWidget label = new LabelWidget(x, y + 7, -1, -1, 0xFFB0B0B0, "minihud.gui.button.shapes.main_rendering");
        this.addWidget(label);

        x += label.getWidth() + 2;
        GenericButton button = new BooleanConfigButton(x, y, -1, 20, Configs.Generic.MAIN_RENDERING_TOGGLE);
        this.addWidget(button);

        x += button.getWidth() + 10;
        label = new LabelWidget(x, y + 7, -1, -1, 0xFFB0B0B0, "minihud.gui.button.shapes.shape_renderer");
        this.addWidget(label);

        x += label.getWidth() + 2;
        button = new BooleanConfigButton(x, y, -1, 20, RendererToggle.SHAPE_RENDERER.getBooleanConfig());
        this.addWidget(button);

        Keyboard.enableRepeatEvents(true);
    }

    protected void createTabButtons()
    {
        int x = 10;
        int y = 26;
        int rows = 1;

        for (ConfigTab tab : ConfigScreen.TABS)
        {
            int width = this.getStringWidth(tab.getDisplayName()) + 10;

            if (x >= this.width - width - 10)
            {
                x = 10;
                y += 22;
                rows++;
            }

            x += this.createTabButton(x, y, width, tab);
        }

        if (rows > 1)
        {
            this.updateListPosition(this.getListX(), 68 + (rows - 1) * 22);
            //this.reCreateListWidget();
        }
    }

    protected int createTabButton(int x, int y, int width, final ConfigTab tab)
    {
        GenericButton button = new GenericButton(x, y, width, 20, tab.getDisplayName());
        button.setEnabled(tab != ConfigScreen.SHAPES);
        this.addButton(button, (btn, mbtn) -> BaseScreen.openGui(ConfigScreen.createOnTab(tab)));

        return button.getWidth() + 2;
    }

    protected int addShapeAddButton(int x, int y)
    {
        GenericButton button = new GenericButton(x, y, -1, true, "minihud.gui.button.add_shape");
        this.addButton(button, (btn, mbtn) -> {
            ShapeType type = this.widgetDropDown.getSelectedEntry();

            if (type != null)
            {
                ShapeManager.INSTANCE.addShape(type.createShape());
                this.getListWidget().refreshEntries();
            }
            else
            {
                MessageUtils.showGuiMessage(MessageType.ERROR, "minihud.message.error.shapes.select_shape_from_dropdown");
            }
        });

        return button.getWidth();
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
        listWidget.setContentsAreDynamic(true);

        DataListEntrySelectionHandler<ShapeBase> handler = new DataListEntrySelectionHandler<>(listWidget::getFilteredEntries);
        handler.setSelectionListener(this::onSelectionChange);
        listWidget.setEntrySelectionHandler(handler);

        return listWidget;
    }
}
