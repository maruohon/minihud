package fi.dy.masa.minihud;

import fi.dy.masa.malilib.config.BaseModConfig;
import fi.dy.masa.malilib.event.InitializationHandler;
import fi.dy.masa.malilib.gui.config.ConfigSearchInfo;
import fi.dy.masa.malilib.registry.Registry;
import fi.dy.masa.minihud.config.ConfigCallbacks;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.InfoLine;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.config.StructureToggle;
import fi.dy.masa.minihud.event.ClientTickHandler;
import fi.dy.masa.minihud.event.ClientWorldChangeHandler;
import fi.dy.masa.minihud.event.HotkeyProvider;
import fi.dy.masa.minihud.event.RenderHandler;
import fi.dy.masa.minihud.gui.ConfigScreen;
import fi.dy.masa.minihud.gui.widget.InfoLineConfigWidget;
import fi.dy.masa.minihud.gui.widget.RendererToggleConfigWidget;
import fi.dy.masa.minihud.gui.widget.StructureToggleConfigWidget;
import fi.dy.masa.minihud.gui.widget.info.RendererToggleConfigStatusWidget;
import fi.dy.masa.minihud.gui.widget.info.StructureRendererConfigStatusWidget;
import fi.dy.masa.minihud.hotkeys.Actions;
import fi.dy.masa.minihud.network.ServuxInfoSubDataPacketHandler;
import fi.dy.masa.minihud.network.ServuxInfoSubRegistrationPacketHandler;

public class InitHandler implements InitializationHandler
{
    @Override
    public void registerModHandlers()
    {
        Registry.CONFIG_MANAGER.registerConfigHandler(BaseModConfig.createDefaultModConfig(Reference.MOD_INFO, Configs.CONFIG_VERSION, Configs.CATEGORIES));
        Registry.CONFIG_SCREEN.registerConfigScreenFactory(Reference.MOD_INFO, ConfigScreen::create);
        Registry.CONFIG_TAB.registerConfigTabProvider(Reference.MOD_INFO, ConfigScreen::getConfigTabs);

        Registry.CONFIG_WIDGET.registerConfigWidgetFactory(InfoLine.class, InfoLineConfigWidget::new);
        Registry.CONFIG_WIDGET.registerConfigWidgetFactory(RendererToggle.class, RendererToggleConfigWidget::new);
        Registry.CONFIG_WIDGET.registerConfigWidgetFactory(StructureToggle.class, StructureToggleConfigWidget::new);

        Registry.CONFIG_WIDGET.registerConfigSearchInfo(InfoLine.class, new ConfigSearchInfo<InfoLine>(true, true).setBooleanConfigGetter(InfoLine::getBooleanConfig).setKeyBindGetter(InfoLine::getKeyBind));
        Registry.CONFIG_WIDGET.registerConfigSearchInfo(RendererToggle.class, new ConfigSearchInfo<RendererToggle>(true, true).setBooleanConfigGetter(RendererToggle::getBooleanConfig).setKeyBindGetter(RendererToggle::getKeyBind));
        Registry.CONFIG_WIDGET.registerConfigSearchInfo(StructureToggle.class, new ConfigSearchInfo<StructureToggle>(true, true).setBooleanConfigGetter(StructureToggle::getBooleanConfig).setKeyBindGetter(StructureToggle::getKeyBind));

        Registry.CONFIG_STATUS_WIDGET.registerConfigStatusWidgetFactory(RendererToggle.class, RendererToggleConfigStatusWidget::new, "minihud:csi_value_renderer_toggle");
        Registry.CONFIG_STATUS_WIDGET.registerConfigStatusWidgetFactory(StructureToggle.class, StructureRendererConfigStatusWidget::new, "minihud:csi_value_structure_toggle");

        Registry.HOTKEY_MANAGER.registerHotkeyProvider(HotkeyProvider.INSTANCE);

        RenderHandler renderer = RenderHandler.INSTANCE;
        Registry.RENDER_EVENT_DISPATCHER.registerGameOverlayRenderer(renderer);
        Registry.RENDER_EVENT_DISPATCHER.registerTooltipPostRenderer(renderer);
        Registry.RENDER_EVENT_DISPATCHER.registerWorldPostRenderer(renderer);

        Registry.CLIENT_WORLD_CHANGE_EVENT_DISPATCHER.registerClientWorldChangeHandler(new ClientWorldChangeHandler());

        Registry.TICK_EVENT_DISPATCHER.registerClientTickHandler(new ClientTickHandler());

        Registry.CLIENT_PACKET_CHANNEL_HANDLER.registerClientChannelHandler(ServuxInfoSubRegistrationPacketHandler.INSTANCE);
        Registry.CLIENT_PACKET_CHANNEL_HANDLER.registerClientChannelHandler(ServuxInfoSubDataPacketHandler.INSTANCE);

        Actions.init();
        ConfigCallbacks.init();
    }
}
