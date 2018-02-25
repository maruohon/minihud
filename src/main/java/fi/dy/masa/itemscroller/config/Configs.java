package fi.dy.masa.itemscroller.config;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import com.mumfrey.liteloader.core.LiteLoader;
import fi.dy.masa.itemscroller.LiteModItemScroller;
import fi.dy.masa.itemscroller.Reference;
import fi.dy.masa.itemscroller.util.JsonUtils;

public class Configs
{
    private static final String CONFIG_FILE_NAME = Reference.MOD_ID + ".json";
    public static final Gson GSON = new GsonBuilder().create();

    public enum Toggles
    {
        ALT_CLICK_MATCHING                      ("enableAltClickMoveMatchingStacks",   true),
        ALT_SHIFT_CLICK_EVERYTHING              ("enableAltShiftClickMoveEverything",  true),
        CONTROL_SHIFT_DROP                      ("enableControlShiftDropkeyDropItems", true),
        DRAG_MOVE_SHIFT_LEFT                    ("enableDragMovingShiftLeft",          true),
        DRAG_MOVE_SHIFT_RIGHT                   ("enableDragMovingShiftRight",         true),
        DRAG_MOVE_CONTROL_LEFT                  ("enableDragMovingControlLeft",        true),
        REVERSE_SCROLL_DIRECTION_SINGLE         ("reverseScrollDirectionSingle",       false),
        REVERSE_SCROLL_DIRECTION_STACKS         ("reverseScrollDirectionStacks",       false),
        RIGHT_CLICK_CRAFT_STACK                 ("enableRightClickCraftingOneStack",   true),
        SCROLL_CRAFT                            ("enableScrollingCrafting",            true),
        //SCROLL_CRAFT_STORE_RECIPE_ON_FILL       ("craftingScrollingStoreRecipeOnFill", true),
        SCROLL_CRAFT_STORE_RECIPES_TO_FILE      ("craftingScrollingSaveToFile",        true),
        SCROLL_CRAFT_RECIPE_FILE_GLOBAL         ("craftingScrollingSaveFileIsGlobal",  false),
        SCROLL_EVERYTHING                       ("enableScrollingEverything",          true),
        SCROLL_MATCHING                         ("enableScrollingMatchingStacks",      true),
        SCROLL_SINGLE                           ("enableScrollingSingle",              true),
        SCROLL_STACKS                           ("enableScrollingStacks",              true),
        SCROLL_STACKS_FALLBACK                  ("enableScrollingStacksFallback",      true),
        SCROLL_VILLAGER                         ("enableScrollingVillager",            true),
        SHIFT_DROP_ITEMS                        ("enableShiftDropItems",               true),
        SHIFT_PLACE_ITEMS                       ("enableShiftPlaceItems",              true),
        SLOT_POSITION_AWARE_SCROLL_DIRECTION    ("useSlotPositionAwareScrollDirection",false);

        private final String name;
        private String comment;
        private boolean value;

        private Toggles(String name, boolean defaultValue)
        {
            this.name = name;
            this.value = defaultValue;
        }

        public String getName()
        {
            return this.name;
        }

        public String getComment()
        {
            return comment != null ? comment : "";
        }

        public void setComment(String comment)
        {
            this.comment = comment;
        }

        public boolean getValue()
        {
            return this.value;
        }

        public void setValue(boolean value)
        {
            this.value = value;
        }
    }

    public static final Set<String> GUI_BLACKLIST = new HashSet<String>();
    public static final Set<String> SLOT_BLACKLIST = new HashSet<String>();

    static
    {
        Toggles.ALT_CLICK_MATCHING.setComment("Enable Alt + click to move all matching stacks\n" +
                                              "(same as the Ctrl + scroll functionality).");
        Toggles.ALT_SHIFT_CLICK_EVERYTHING.setComment("Enable Alt + Shift + click to move everything\n" +
                                                      "(same as the Ctrl + Shift + scroll functionality).");
        Toggles.CONTROL_SHIFT_DROP.setComment("Enable dropping all matching items from the same inventory\n" +
                                              "when pressing Ctrl + Shift + the drop item key.");
        Toggles.DRAG_MOVE_SHIFT_LEFT.setComment("Enable moving full stacks of items by holding down\n" +
                                                "Shift and dragging over slots with the left mouse button held down.");
        Toggles.DRAG_MOVE_SHIFT_RIGHT.setComment("Enable moving everything but the last item from all stacks by holding\n" +
                                                 "down Shift and dragging over slots with the right mouse button held down.");
        Toggles.DRAG_MOVE_CONTROL_LEFT.setComment("Enable moving one item from all stacks by holding down Control and\n" +
                                                  "dragging over slots with the left mouse button held down.");
        Toggles.REVERSE_SCROLL_DIRECTION_SINGLE.setComment("Reverse the scrolling direction for single item mode.");
        Toggles.REVERSE_SCROLL_DIRECTION_STACKS.setComment("Reverse the scrolling direction for full stacks mode.");
        Toggles.RIGHT_CLICK_CRAFT_STACK.setComment("Enable crafting up to one full stack when right clicking on\n" +
                                                   "a slot that has been configured as a crafting slot.");
        Toggles.SCROLL_CRAFT_RECIPE_FILE_GLOBAL.setComment("If true, then a single file is used for storing the recipes,\n" +
                                                           "instead of per-world or per-server files.");
        Toggles.SCROLL_CRAFT_STORE_RECIPES_TO_FILE.setComment("Enables saving and loading the stored recipes to a file\n" +
                                                              "inside minecraft/itemscroller/recipes_worldorservername.nbt,\n" +
                                                              "so that they are persistent between game restarts.");
        Toggles.SHIFT_DROP_ITEMS.setComment("Enable dropping items while holding shift to drop\nall the matching items at once.");
        Toggles.SHIFT_PLACE_ITEMS.setComment("Enable placing items to an empty slot while holding shift\n" +
                                             "to move all the matching items to that inventory.");
        Toggles.SCROLL_CRAFT.setComment("Enable scrolling items to and from crafting grids, with a built-in 18 recipe memory.\n" +
                                        "Hold down the Recipe key to see the stored recipes and to change the selection.\n" +
                                        "While holding the Recipe key, you can either scroll or\n" +
                                        "press a number key to change the selection.\n" +
                                        "A recipe is stored to the currently selected \"recipe slot\"\n" +
                                        "by scrolling over the output slot, or by pressing\n" +
                                        "Shift + the Recipe key + a number key.\n" +
                                        "The supported crafting grids must be added to the scrollableCraftingGrids list.");
        Toggles.SCROLL_EVERYTHING.setComment("Enable moving all items at once (while holding Ctrl and Shift and scrolling).");
        Toggles.SCROLL_MATCHING.setComment("Enable moving all matching items at once (while holding Ctrl and scrolling).");
        Toggles.SCROLL_SINGLE.setComment("Enable scrolling items one item at a time.");
        Toggles.SCROLL_STACKS.setComment("Enable scrolling full stack (while holding Shift and scrolling).");
        Toggles.SCROLL_STACKS_FALLBACK.setComment("Enable a \"fallback\" mode for scrolling entire stacks\n" +
                                                  "(for example to a vanilla crafting table, where shift + click doesn't work).");
        Toggles.SCROLL_VILLAGER.setComment("Enable special handling for Villager GUI\n" +
                                           "(normally you can't shift+click items into them).\n" +
                                           "Hold shift and scroll up/down over the trade output slot.");
        Toggles.SLOT_POSITION_AWARE_SCROLL_DIRECTION.setComment("When enabled, the item movement direction depends\n" +
                                                                "on the slots' y-position on screen. Might be derpy with more\n" +
                                                                "complex inventories, use with caution!");
    }

    public static void load()
    {
        File configFile = new File(LiteLoader.getCommonConfigFolder(), CONFIG_FILE_NAME);

        if (configFile.exists() && configFile.isFile() && configFile.canRead())
        {
            JsonElement element = JsonUtils.parseJsonFile(configFile);

            if (element != null && element.isJsonObject())
            {
                JsonObject root = element.getAsJsonObject();

                for (Toggles toggle : Toggles.values())
                {
                    if (JsonUtils.hasBoolean(root, toggle.getName()))
                    {
                        toggle.setValue(JsonUtils.getBoolean(root, toggle.getName()));
                    }
                }

                getStrings(root, GUI_BLACKLIST, "guiBlacklist");
                getStrings(root, SLOT_BLACKLIST, "slotBlacklist");
            }
        }
    }

    public static void save()
    {
        File dir = LiteLoader.getCommonConfigFolder();

        if (dir.exists() && dir.isDirectory())
        {
            File configFile = new File(dir, CONFIG_FILE_NAME);
            FileWriter writer = null;
            JsonObject root = new JsonObject();

            for (Toggles toggle : Toggles.values())
            {
                root.add(toggle.getName(), new JsonPrimitive(toggle.getValue()));
            }

            writeStrings(root, GUI_BLACKLIST, "guiBlacklist");
            writeStrings(root, SLOT_BLACKLIST, "slotBlacklist");

            try
            {
                writer = new FileWriter(configFile);
                writer.write(JsonUtils.GSON.toJson(root));
                writer.close();
            }
            catch (IOException e)
            {
                LiteModItemScroller.logger.warn("Failed to write configs to file '{}'", configFile.getAbsolutePath(), e);
            }
            finally
            {
                try
                {
                    if (writer != null)
                    {
                        writer.close();
                    }
                }
                catch (Exception e)
                {
                    LiteModItemScroller.logger.warn("Failed to close config file", e);
                }
            }
        }
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
