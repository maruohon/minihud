package fi.dy.masa.minihud.gui;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.Nullable;
import org.lwjgl.input.Keyboard;
import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.gui.BaseListScreen;
import fi.dy.masa.malilib.gui.BaseScreen;
import fi.dy.masa.malilib.gui.config.BaseConfigScreen;
import fi.dy.masa.malilib.gui.config.ConfigTab;
import fi.dy.masa.malilib.gui.widget.CyclableContainerWidget;
import fi.dy.masa.malilib.gui.widget.DropDownListWidget;
import fi.dy.masa.malilib.gui.widget.LabelWidget;
import fi.dy.masa.malilib.gui.widget.button.BaseButton;
import fi.dy.masa.malilib.gui.widget.button.BooleanConfigButton;
import fi.dy.masa.malilib.gui.widget.button.GenericButton;
import fi.dy.masa.malilib.gui.widget.list.DataListWidget;
import fi.dy.masa.malilib.gui.widget.list.entry.DataListEntrySelectionHandler;
import fi.dy.masa.malilib.render.message.MessageType;
import fi.dy.masa.malilib.render.message.MessageUtils;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.gui.widget.WidgetShapeEntry;
import fi.dy.masa.minihud.renderer.shapes.ShapeBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager;
import fi.dy.masa.minihud.renderer.shapes.ShapeType;

public class GuiShapeManager extends BaseListScreen<DataListWidget<ShapeBase>>
{
    protected final DropDownListWidget<ShapeType> widgetDropDown;
    @Nullable protected CyclableContainerWidget tabButtonContainerWidget;

    public GuiShapeManager()
    {
        super(10, 82, 20, 88);

        this.title = StringUtils.translate("minihud.gui.title.shape_manager");

        // The position will get updated later
        this.widgetDropDown = new DropDownListWidget<>(0, 0, 160, 16, 200, 10, ImmutableList.copyOf(ShapeType.values()), ShapeType::getDisplayName);
        this.widgetDropDown.setZLevel((int) this.zLevel + 2);
    }

    @Override
    public void initGui()
    {
        super.initGui();

        this.clearWidgets();
        this.clearButtons();
        this.createTabButtonWidget();

        int x = 12;
        int y = 44;
        int lw = StringUtils.getMaxStringRenderWidth(StringUtils.translate("minihud.gui.button.shapes.main_rendering"),
                                                     StringUtils.translate("minihud.gui.button.shapes.shape_renderer"));

        LabelWidget label = new LabelWidget(x, y + 5, lw, -1, 0xFFFFF040, "minihud.gui.button.shapes.main_rendering");
        this.addWidget(label);

        x += label.getWidth() + 4;
        GenericButton button = new BooleanConfigButton(x, y, -1, 18, Configs.Generic.MAIN_RENDERING_TOGGLE);
        this.addWidget(button);

        this.widgetDropDown.setPosition(this.width - 10, y);
        this.widgetDropDown.setRightAlign(true, this.width - 10, true);
        this.addWidget(this.widgetDropDown);

        y += 18;
        x = 12;
        label = new LabelWidget(x, y + 6, lw, -1, 0xFFFFF040, "minihud.gui.button.shapes.shape_renderer");
        this.addWidget(label);

        x += label.getWidth() + 4;
        button = new BooleanConfigButton(x, y + 1, -1, 18, RendererToggle.SHAPE_RENDERER.getBooleanConfig());
        this.addWidget(button);

        button = new GenericButton(this.width - 10, y, -1, true, "minihud.gui.button.add_shape");
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

        Keyboard.enableRepeatEvents(true);
    }

    @Override
    public void onGuiClosed()
    {
        super.onGuiClosed();

        if (this.tabButtonContainerWidget != null)
        {
            BaseConfigScreen.getTabState(Reference.MOD_ID).currentTabStartIndex = this.tabButtonContainerWidget.getStartIndex();
        }
    }

    protected void createTabButtonWidget()
    {
        this.tabButtonContainerWidget = new CyclableContainerWidget(10, 22, this.width - 20, 20, this.createTabButtons());
        this.tabButtonContainerWidget.setStartIndex(BaseConfigScreen.getTabState(Reference.MOD_ID).currentTabStartIndex);
        this.addWidget(this.tabButtonContainerWidget);
    }

    protected List<BaseButton> createTabButtons()
    {
        List<BaseButton> buttons = new ArrayList<>();

        for (ConfigTab tab : ConfigScreen.TABS)
        {
            buttons.add(this.createTabButton(tab));
        }

        return buttons;
    }

    protected GenericButton createTabButton(ConfigTab tab)
    {
        GenericButton button = new GenericButton(0, 0, -1, 20, tab.getDisplayName());
        button.setEnabled(ConfigScreen.SHAPES != tab);
        button.setActionListener((btn, mbtn) -> BaseScreen.openGui(ConfigScreen.createOnTab(tab)));
        return button;
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
