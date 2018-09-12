package fi.dy.masa.itemscroller.config;

import java.io.File;
import java.util.HashSet;
import java.util.Set;
import com.google.common.collect.ImmutableList;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.mumfrey.liteloader.core.LiteLoader;
import fi.dy.masa.itemscroller.Reference;
import fi.dy.masa.itemscroller.recipes.CraftingHandler;
import fi.dy.masa.itemscroller.recipes.CraftingHandler.SlotRange;
import fi.dy.masa.malilib.config.ConfigUtils;
import fi.dy.masa.malilib.config.IConfigHandler;
import fi.dy.masa.malilib.config.IConfigValue;
import fi.dy.masa.malilib.config.options.ConfigBoolean;
import fi.dy.masa.malilib.util.JsonUtils;
import net.minecraft.client.gui.inventory.GuiCrafting;
import net.minecraft.client.gui.inventory.GuiInventory;
import net.minecraft.inventory.SlotCrafting;

public class Configs implements IConfigHandler
{
    private static final String CONFIG_FILE_NAME = Reference.MOD_ID + ".json";

    public static class Generic
    {
        public static final ConfigBoolean CARPET_CTRL_Q_CRAFTING                = new ConfigBoolean("carpetCtrlQCraftingEnabledOnServer",   false, "Set to true if the server is running the Carpet mod,\nand has the ctrlQCrafting option enabled.\nThis just changes which method Item Scroller uses\nfor the Drop key + Shift + Right click crafting.");
        public static final ConfigBoolean CLIENT_CRAFTING_FIX                   = new ConfigBoolean("clientCraftingFixOn1.12",              true, "Enable updating the crafting recipe output directly on the client side.\nThis fixes the quick/mass crafting and right-click-to-craft-a-stack\nfeatures othrwise being broken in 1.12.");
        public static final ConfigBoolean CRAFTING_RENDER_RECIPE_ITEMS          = new ConfigBoolean("craftingRenderRecipeItems",            true, "If enabled, then the recipe items are also rendered\nin the crafting recipe view.");
        public static final ConfigBoolean SCROLL_CRAFT_STORE_RECIPES_TO_FILE    = new ConfigBoolean("craftingRecipesSaveToFile",            true, "If enabled, then the crafting features recipes are saved to a file\ninside minecraft/itemscroller/recipes_worldorservername.nbt.\nThis makes the recipes persistent across game restarts.");
        public static final ConfigBoolean SCROLL_CRAFT_RECIPE_FILE_GLOBAL       = new ConfigBoolean("craftingRecipesSaveFileIsGlobal",      false, "If true, then the recipe file is global, instead\n of being saved per-world or server");
        public static final ConfigBoolean REVERSE_SCROLL_DIRECTION_SINGLE       = new ConfigBoolean("reverseScrollDirectionSingle",         false, "Reverse the scrolling direction for single item mode.");
        public static final ConfigBoolean REVERSE_SCROLL_DIRECTION_STACKS       = new ConfigBoolean("reverseScrollDirectionStacks",         false, "Reverse the scrolling direction for full stacks mode.");
        public static final ConfigBoolean SLOT_POSITION_AWARE_SCROLL_DIRECTION  = new ConfigBoolean("useSlotPositionAwareScrollDirection",  false, "When enabled, the item movement direction depends\non the slots' y-position on screen. Might be derpy with more\ncomplex inventories, use with caution!");

        public static final ImmutableList<IConfigValue> OPTIONS = ImmutableList.of(
                CARPET_CTRL_Q_CRAFTING,
                CLIENT_CRAFTING_FIX,
                CRAFTING_RENDER_RECIPE_ITEMS,
                SCROLL_CRAFT_STORE_RECIPES_TO_FILE,
                SCROLL_CRAFT_RECIPE_FILE_GLOBAL,
                REVERSE_SCROLL_DIRECTION_SINGLE,
                REVERSE_SCROLL_DIRECTION_STACKS,
                SLOT_POSITION_AWARE_SCROLL_DIRECTION
        );
    }

    public static class Toggles
    {
        public static final ConfigBoolean CLICK_MOVE_MATCHING       = new ConfigBoolean("enableClickMovingMatchingStacks",  true, "Enables moving all matching stacks while clicking\nwith the modifier key active.");
        public static final ConfigBoolean CLICK_MOVE_EVERYTHING     = new ConfigBoolean("enableClickMovingEverything",      true, "Enables moving all items while clicking\nwith the modifier key active.");
        public static final ConfigBoolean CRAFTING_FEATURES         = new ConfigBoolean("enableCraftingFeatures",           true, "Enables scrolling items to and from crafting grids,\nwith a built-in 18 recipe memory.\nHold down the Recipe key to see the stored recipes and\nto change the selection. While holding the Recipe key,\nyou can either scroll or press a number key to change the selection.\nA recipe is stored to the currently selected \"recipe slot\"\n by clicking pick block over a configured crafting output slot.\nThe supported crafting grids must be added to the scrollableCraftingGrids list.");
        public static final ConfigBoolean DROP_MATCHING             = new ConfigBoolean("enableDropkeyDropMatching",        true, "Enables dropping all matching items from the same\ninventory with the hotkey");
        public static final ConfigBoolean DRAG_DROP_SINGLE          = new ConfigBoolean("enableDragDropSingle",             true, "Enables dropping single items when holding down the\nkeyDragDropSingle key and then dragging over slots");
        public static final ConfigBoolean DRAG_DROP_STACKS          = new ConfigBoolean("enableDragDropStacks",             true, "Enables dropping entire stacks when holding down the\nkeyDragDropStacks key and then dragging over slots");
        public static final ConfigBoolean DRAG_MOVE_STACKS          = new ConfigBoolean("enableDragMovingFullStacks",       true, "Enables moving entire stacks of items by holding down the\nkeyDragMoveStacks key and dragging over slots");
        public static final ConfigBoolean DRAG_MOVE_LEAVE_ONE       = new ConfigBoolean("enableDragMovingLeaveOne",         true, "Enables moving everything but the last item\nfrom all stacks by holding down the keyDragMoveLeaveOne\nkey and dragging over slots");
        public static final ConfigBoolean DRAG_MOVE_ONE             = new ConfigBoolean("enableDragMovingMoveOne",          true, "Enables moving one item from all stacks by holding down the\nkeyDragMoveOne key and dragging over slots");
        public static final ConfigBoolean OFFHAND_SWAP              = new ConfigBoolean("enableOffHandSwap",                true, "Enables using the moveStackToOffhand key to swap\nthe hovered stack to the offhand while\nin the regular player inventory");
        public static final ConfigBoolean RIGHT_CLICK_CRAFT_STACK   = new ConfigBoolean("enableRightClickCraftingOneStack", true, "Enables crafting up to one full stack when right clicking on\na slot that has been configured as a crafting output slot.");
        public static final ConfigBoolean SCROLL_EVERYTHING         = new ConfigBoolean("enableScrollingEverything",        true, "Enables scroll moving all items at once while\nholding the modifierMoveEverything keybind");
        public static final ConfigBoolean SCROLL_MATCHING           = new ConfigBoolean("enableScrollingMatchingStacks",    true, "Enables scroll moving all matching stacks at once\nwhile holding the modifierMoveMatching keybind");
        public static final ConfigBoolean SCROLL_SINGLE             = new ConfigBoolean("enableScrollingSingle",            true, "Enables moving items one item at a time by scrolling over a stack");
        public static final ConfigBoolean SCROLL_STACKS             = new ConfigBoolean("enableScrollingStacks",            true, "Enables moving entire stacks at a time by scrolling over a stack");
        public static final ConfigBoolean SCROLL_STACKS_FALLBACK    = new ConfigBoolean("enableScrollingStacksFallback",    true, "Enables a \"fallback\" mode for scrolling entire stacks\n(for example to a vanilla crafting table,\nwhere shift + click doesn't work).");
        public static final ConfigBoolean SCROLL_VILLAGER           = new ConfigBoolean("enableScrollingVillager",          true, "Enables special handling for the Villager GUIs.\n(Normally you can't shift click items in them.)\nHold shift and scroll up/down over the trade output slot.");
        public static final ConfigBoolean SHIFT_DROP_ITEMS          = new ConfigBoolean("enableShiftDropItems",             true, "Enables dropping all matching items at once by holding\nshift while clicking to drop a stack");
        public static final ConfigBoolean SHIFT_PLACE_ITEMS         = new ConfigBoolean("enableShiftPlaceItems",            true, "Enables moving all matching stacks at once by holding\nshift while placing items to an empty slot");
        public static final ConfigBoolean WS_CLICKING               = new ConfigBoolean("enableWSClicking",                 true, "Enables moving items up or down in the inventory while\nholding down W or S and then clicking (+ dragging)");

        public static final ImmutableList<IConfigValue> OPTIONS = ImmutableList.of(
                CLICK_MOVE_MATCHING,
                CLICK_MOVE_EVERYTHING,
                CRAFTING_FEATURES,
                DROP_MATCHING,
                DRAG_DROP_SINGLE,
                DRAG_DROP_STACKS,
                DRAG_MOVE_STACKS,
                DRAG_MOVE_LEAVE_ONE,
                DRAG_MOVE_ONE,
                OFFHAND_SWAP,
                RIGHT_CLICK_CRAFT_STACK,
                SCROLL_EVERYTHING,
                SCROLL_MATCHING,
                SCROLL_SINGLE,
                SCROLL_STACKS,
                SCROLL_STACKS_FALLBACK,
                SCROLL_VILLAGER,
                SHIFT_DROP_ITEMS,
                SHIFT_PLACE_ITEMS,
                WS_CLICKING
        );
    }

    public static final Set<String> GUI_BLACKLIST = new HashSet<String>();
    public static final Set<String> SLOT_BLACKLIST = new HashSet<String>();

    public static void loadFromFile()
    {
        File configFile = new File(LiteLoader.getCommonConfigFolder(), CONFIG_FILE_NAME);

        if (configFile.exists() && configFile.isFile() && configFile.canRead())
        {
            JsonElement element = JsonUtils.parseJsonFile(configFile);

            if (element != null && element.isJsonObject())
            {
                JsonObject root = element.getAsJsonObject();

                ConfigUtils.readConfigBase(root, "Generic", Generic.OPTIONS);
                ConfigUtils.readConfigBase(root, "Toggles", Toggles.OPTIONS);
                ConfigUtils.readHotkeys(root, "Hotkeys", Hotkeys.HOTKEY_LIST);

                getStrings(root, GUI_BLACKLIST, "guiBlacklist");
                getStrings(root, SLOT_BLACKLIST, "slotBlacklist");
            }
        }

        CraftingHandler.clearDefinitions();

        // "net.minecraft.client.gui.inventory.GuiCrafting,net.minecraft.inventory.SlotCrafting,0,1-9", // vanilla Crafting Table
        CraftingHandler.addCraftingGridDefinition(GuiCrafting.class.getName(), SlotCrafting.class.getName(), 0, new SlotRange(1, 9));
        //"net.minecraft.client.gui.inventory.GuiInventory,net.minecraft.inventory.SlotCrafting,0,1-4", // vanilla player inventory crafting grid
        CraftingHandler.addCraftingGridDefinition(GuiInventory.class.getName(), SlotCrafting.class.getName(), 0, new SlotRange(1, 4));
    }

    public static void saveToFile()
    {
        File dir = LiteLoader.getCommonConfigFolder();

        if (dir.exists() && dir.isDirectory())
        {
            JsonObject root = new JsonObject();

            ConfigUtils.writeConfigBase(root, "Generic", Generic.OPTIONS);
            ConfigUtils.writeConfigBase(root, "Toggles", Toggles.OPTIONS);
            ConfigUtils.writeHotkeys(root, "Hotkeys", Hotkeys.HOTKEY_LIST);

            writeStrings(root, GUI_BLACKLIST, "guiBlacklist");
            writeStrings(root, SLOT_BLACKLIST, "slotBlacklist");

            JsonUtils.writeJsonToFile(root, new File(dir, CONFIG_FILE_NAME));
        }
    }

    @Override
    public void onConfigsChanged()
    {
        saveToFile();
        loadFromFile();
    }

    @Override
    public void save()
    {
        saveToFile();
    }

    private static void getStrings(JsonObject obj, Set<String> outputSet, String arrayName)
    {
        outputSet.clear();

        if (JsonUtils.hasArray(obj, arrayName))
        {
            JsonArray arr = obj.getAsJsonArray(arrayName);
            final int size = arr.size();

            for (int i = 0; i < size; i++)
            {
                outputSet.add(arr.get(i).getAsString());
            }
        }
    }

    private static void writeStrings(JsonObject obj, Set<String> inputSet, String arrayName)
    {
        if (inputSet.isEmpty() == false)
        {
            JsonArray arr = new JsonArray();

            for (String str : inputSet)
            {
                arr.add(str);
            }

            obj.add(arrayName, arr);
        }
    }
}
