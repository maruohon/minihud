package fi.dy.masa.itemscroller.config;

import java.util.List;
import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.options.ConfigHotkey;
import fi.dy.masa.malilib.hotkeys.KeybindSettings;

public class Hotkeys
{
    public static final ConfigHotkey KEY_CRAFT_EVERYTHING       = new ConfigHotkey("craftEverything",   "LCONTROL,C", KeybindSettings.GUI, "Craft everything possible with the currently selected recipe");
    public static final ConfigHotkey KEY_DROP_ALL_MATCHING      = new ConfigHotkey("dropAllMatching",   "LCONTROL,LSHIFT,Q", KeybindSettings.GUI, "Drop all stacks identical to the hovered stack");
    public static final ConfigHotkey KEY_MAIN_TOGGLE            = new ConfigHotkey("mainToggle",        "LCONTROL,S", KeybindSettings.GUI, "Toggle all functionality ON/OFF");
    public static final ConfigHotkey KEY_MASS_CRAFT             = new ConfigHotkey("massCraft",         "LCONTROL,LMENU,C", KeybindSettings.GUI, "Mass craft and throw out the results with the\ncurrently selected recipe as long as this\nkeybind is held down");
    public static final ConfigHotkey KEY_MOVE_CRAFT_RESULTS     = new ConfigHotkey("moveCraftResults",  "LCONTROL,M", KeybindSettings.GUI, "Move all of the currently selected recipe's\noutput items from the player inventory\nto the other inventory");
    public static final ConfigHotkey KEY_MOVE_STACK_TO_OFFHAND  = new ConfigHotkey("moveStackToOffhand", "F", KeybindSettings.GUI, "Swap the hovered stack with the offhand");
    public static final ConfigHotkey KEY_RECIPE_VIEW            = new ConfigHotkey("recipeView",        "A", KeybindSettings.GUI, "Show the Item Scroller recipe GUI");
    public static final ConfigHotkey KEY_SLOT_DEBUG             = new ConfigHotkey("slotDebug",         "LCONTROL,LMENU,LSHIFT,I", KeybindSettings.GUI, "Print debug info for the hovered slot or GUI");
    public static final ConfigHotkey KEY_THROW_CRAFT_RESULTS    = new ConfigHotkey("throwCraftResults", "LCONTROL,T", KeybindSettings.GUI, "Throw all of the currently selected recipe's\noutput items to the ground from the player inventory");

    public static final ConfigHotkey KEY_DRAG_DROP_SINGLE       = new ConfigHotkey("keyDragDropSingle",   "LCONTROL,Q,BUTTON0", KeybindSettings.GUI, "Key to drop one item from each stack dragged over");
    public static final ConfigHotkey KEY_DRAG_DROP_STACKS       = new ConfigHotkey("keyDragDropStacks",   "LSHIFT,Q,BUTTON0", KeybindSettings.GUI, "Key to drop the entire stacks dragged over");
    public static final ConfigHotkey KEY_DRAG_FULL_STACKS       = new ConfigHotkey("keyDragMoveStacks",   "LSHIFT,BUTTON0", KeybindSettings.GUI, "Key to move the entire stacks dragged over");
    public static final ConfigHotkey KEY_DRAG_LEAVE_ONE         = new ConfigHotkey("keyDragMoveLeaveOne", "LSHIFT,BUTTON1", KeybindSettings.GUI, "Key to move all but the last item from\nall the stacks dragged over");
    public static final ConfigHotkey KEY_DRAG_MOVE_ONE          = new ConfigHotkey("keyDragMoveOne",      "LCONTROL,BUTTON0", KeybindSettings.GUI, "Key to move one item from each stack dragged over");

    public static final ConfigHotkey MODIFIER_MOVE_EVERYTHING   = new ConfigHotkey("modifierMoveEverything", "LMENU,LSHIFT", KeybindSettings.GUI, "Modifier key to move ALL items to the other\ninventory when clicking on a stack or scrolling over it");
    public static final ConfigHotkey MODIFIER_MOVE_MATCHING     = new ConfigHotkey("modifierMoveMatching",   "LCONTROL", KeybindSettings.GUI, "Modifier key to move all matching items to the other\ninventory when clicking on a stack or scrolling over it");
    public static final ConfigHotkey MODIFIER_MOVE_STACK        = new ConfigHotkey("modifierMoveStack",      "LSHIFT", KeybindSettings.GUI, "Modifier key to move the entire stack when\nclicking on the stack or scrolling over it");

    public static final List<ConfigHotkey> HOTKEY_LIST = ImmutableList.of(
            MODIFIER_MOVE_EVERYTHING,
            MODIFIER_MOVE_MATCHING,
            MODIFIER_MOVE_STACK,

            KEY_DRAG_DROP_SINGLE,
            KEY_DRAG_DROP_STACKS,
            KEY_DRAG_FULL_STACKS,
            KEY_DRAG_LEAVE_ONE,
            KEY_DRAG_MOVE_ONE,

            KEY_CRAFT_EVERYTHING,
            KEY_DROP_ALL_MATCHING,
            KEY_MAIN_TOGGLE,
            KEY_MASS_CRAFT,
            KEY_MOVE_CRAFT_RESULTS,
            KEY_MOVE_STACK_TO_OFFHAND,
            KEY_RECIPE_VIEW,
            KEY_SLOT_DEBUG,
            KEY_THROW_CRAFT_RESULTS
    );
}
