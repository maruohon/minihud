package fi.dy.masa.itemscroller.config;

import java.io.File;
import net.minecraftforge.common.config.Configuration;
import net.minecraftforge.common.config.Property;
import net.minecraftforge.fml.client.event.ConfigChangedEvent.OnConfigChangedEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import fi.dy.masa.itemscroller.Reference;

public class Configs
{
    public static boolean enableDragMovingShiftLeft;
    public static boolean enableDragMovingShiftRight;
    public static boolean enableDragMovingControlLeft;
    public static boolean enableScrollingEverything;
    public static boolean enableScrollingMatchingStacks;
    public static boolean enableScrollingSingle;
    public static boolean enableScrollingStacks;
    public static boolean enableScrollingStacksFallback;
    public static boolean enableScrollingVillager;
    public static boolean enableShiftDropItems;
    public static boolean enableShiftPlaceItems;
    public static boolean reverseScrollDirectionSingle;
    public static boolean reverseScrollDirectionStacks;
    public static boolean useSlotPositionAwareScrollDirection;

    public static File configurationFile;
    public static Configuration config;
    
    public static final String CATEGORY_GENERIC = "Generic";

    @SubscribeEvent
    public void onConfigChangedEvent(OnConfigChangedEvent event)
    {
        if (Reference.MOD_ID.equals(event.getModID()) == true)
        {
            loadConfigs(config);
        }
    }

    public static void loadConfigsFromFile(File configFile)
    {
        configurationFile = configFile;
        config = new Configuration(configFile, null, false);
        config.load();

        loadConfigs(config);
    }

    public static void loadConfigs(Configuration conf)
    {
        Property prop;

        prop = conf.get(CATEGORY_GENERIC, "enableDragMovingShiftLeft", true).setRequiresMcRestart(false);
        prop.setComment("Enable moving full stacks of items by holding down Shift and dragging over slots with the left mouse button held down.");
        enableDragMovingShiftLeft = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "enableDragMovingShiftRight", true).setRequiresMcRestart(false);
        prop.setComment("Enable moving everything but the last item from all stacks by holding down Shift and dragging over slots with the right mouse button held down.");
        enableDragMovingShiftRight = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "enableDragMovingControlLeft", true).setRequiresMcRestart(false);
        prop.setComment("Enable moving one item from all stacks by holding down Control and dragging over slots with the left mouse button held down.");
        enableDragMovingControlLeft = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "enableMovingEverything", true).setRequiresMcRestart(false);
        prop.setComment("Enable moving all items at once (while holding ctrl and shift).");
        enableScrollingEverything = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "enableScrollingMatchingStacks", true).setRequiresMcRestart(false);
        prop.setComment("Enable moving all matching items at once (while holding ctrl).");
        enableScrollingMatchingStacks = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "enableScrollingSingle", true).setRequiresMcRestart(false);
        prop.setComment("Enable scrolling items one item at a time.");
        enableScrollingSingle = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "enableScrollingStacks", true).setRequiresMcRestart(false);
        prop.setComment("Enable item scrolling full stack at a time (while holding shift).");
        enableScrollingStacks = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "enableScrollingStacksFallback", true).setRequiresMcRestart(false);
        prop.setComment("Enable a \"fallback\" mode for scrolling entire stacks (for example to a vanilla crafting table, where shift + click doesn't work).");
        enableScrollingStacksFallback = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "enableScrollingVillager", true).setRequiresMcRestart(false);
        prop.setComment("Enable special handling for Villager GUI (normally you can't shift+click items into them).");
        enableScrollingVillager = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "enableShiftDropItems", true).setRequiresMcRestart(false);
        prop.setComment("Enable dropping items while holding shift to drop all the matching items at once.");
        enableShiftDropItems = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "enableShiftPlaceItems", true).setRequiresMcRestart(false);
        prop.setComment("Enable placing items to an empty slot while holding shift to move all the mathing items to that inventory.");
        enableShiftPlaceItems = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "reverseScrollDirectionSingle", false).setRequiresMcRestart(false);
        prop.setComment("Reverse the scrolling direction for single item mode.");
        reverseScrollDirectionSingle = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "reverseScrollDirectionStacks", false).setRequiresMcRestart(false);
        prop.setComment("Reverse the scrolling direction for full stacks mode.");
        reverseScrollDirectionStacks = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "useSlotPositionAwareScrollDirection", false).setRequiresMcRestart(false);
        prop.setComment("When enabled, the item movement direction depends on the slots' y-position on screen. Might be derpy with more complex inventories, use with caution!");
        useSlotPositionAwareScrollDirection = prop.getBoolean();

        if (conf.hasChanged() == true)
        {
            conf.save();
        }
    }
}
