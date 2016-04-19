package fi.dy.masa.itemscroller.config;

import java.io.File;

import net.minecraftforge.common.config.Configuration;
import net.minecraftforge.common.config.Property;
import net.minecraftforge.fml.client.event.ConfigChangedEvent.OnConfigChangedEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;

import fi.dy.masa.itemscroller.Reference;

public class Configs
{
    public static boolean enableDragMoving;
    public static boolean enableMovingEverything;
    public static boolean enableScrollingMatchingStacks;
    public static boolean enableScrollingSingle;
    public static boolean enableScrollingStacks;
    public static boolean enableScrollingVillager;
    public static boolean reverseScrollDirectionSingle;
    public static boolean reverseScrollDirectionStacks;

    public static File configurationFile;
    public static Configuration config;
    
    public static final String CATEGORY_GENERIC = "Generic";

    @SubscribeEvent
    public void onConfigChangedEvent(OnConfigChangedEvent event)
    {
        if (Reference.MOD_ID.equals(event.modID) == true)
        {
            loadConfigs(config);
        }
    }

    public static void loadConfigsFromFile(File configFile)
    {
        configurationFile = configFile;
        config = new Configuration(configFile, "0.2.0", true);
        config.load();

        loadConfigs(config);
    }

    public static void loadConfigs(Configuration conf)
    {
        Property prop = conf.get(CATEGORY_GENERIC, "enableDragMoving", true).setRequiresMcRestart(false);
        prop.comment = "Enable moving items by holding down shift and dragging over slots.";
        enableDragMoving = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "enableMovingEverything", true).setRequiresMcRestart(false);
        prop.comment = "Enable moving all items at once (while holding ctrl and shift).";
        enableMovingEverything = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "enableScrollingMatchingStacks", true).setRequiresMcRestart(false);
        prop.comment = "Enable moving all matching items at once (while holding ctrl).";
        enableScrollingMatchingStacks = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "enableScrollingSingle", true).setRequiresMcRestart(false);
        prop.comment = "Enable scrolling items one item at a time.";
        enableScrollingSingle = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "enableScrollingStacks", true).setRequiresMcRestart(false);
        prop.comment = "Enable item scrolling full stack at a time (while holding shift).";
        enableScrollingStacks = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "enableScrollingVillager", true).setRequiresMcRestart(false);
        prop.comment = "Enable special handling for Villager GUI (normally you can't shift+click items into them).";
        enableScrollingVillager = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "reverseScrollDirectionSingle", false).setRequiresMcRestart(false);
        prop.comment = "Reverse the scrolling direction for single item mode.";
        reverseScrollDirectionSingle = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "reverseScrollDirectionStacks", false).setRequiresMcRestart(false);
        prop.comment = "Reverse the scrolling direction for full stacks mode.";
        reverseScrollDirectionStacks = prop.getBoolean();

        if (conf.hasChanged() == true)
        {
            conf.save();
        }
    }
}
