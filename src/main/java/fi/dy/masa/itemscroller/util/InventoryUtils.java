package fi.dy.masa.itemscroller.util;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.annotation.Nullable;
import fi.dy.masa.itemscroller.ItemScroller;
import fi.dy.masa.itemscroller.config.Configs;
import fi.dy.masa.itemscroller.config.Hotkeys;
import fi.dy.masa.itemscroller.event.InputHandler;
import fi.dy.masa.itemscroller.recipes.CraftingHandler;
import fi.dy.masa.itemscroller.recipes.CraftingHandler.SlotRange;
import fi.dy.masa.itemscroller.recipes.CraftingRecipe;
import fi.dy.masa.itemscroller.recipes.RecipeStorage;
import fi.dy.masa.itemscroller.villager.VillagerData;
import fi.dy.masa.itemscroller.villager.VillagerDataStorage;
import fi.dy.masa.malilib.util.GuiUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.client.entity.EntityPlayerSP;
import net.minecraft.client.gui.GuiMerchant;
import net.minecraft.client.gui.inventory.GuiContainer;
import net.minecraft.client.gui.inventory.GuiContainerCreative;
import net.minecraft.client.gui.inventory.GuiInventory;
import net.minecraft.client.multiplayer.WorldClient;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.InventoryPlayer;
import net.minecraft.inventory.ClickType;
import net.minecraft.inventory.Container;
import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.InventoryCraftResult;
import net.minecraft.inventory.Slot;
import net.minecraft.inventory.SlotMerchantResult;
import net.minecraft.item.ItemGroup;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.IRecipe;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.registry.IRegistry;
import net.minecraft.village.MerchantRecipe;
import net.minecraft.village.MerchantRecipeList;
import net.minecraft.world.World;

public class InventoryUtils
{
    private static MoveAction activeMoveAction = MoveAction.NONE;
    private static int lastPosX;
    private static int lastPosY;
    private static int slotNumberLast;
    private static final Set<Integer> DRAGGED_SLOTS = new HashSet<Integer>();
    private static WeakReference<Slot> sourceSlotCandidate = null;
    private static WeakReference<Slot> sourceSlot = null;
    private static ItemStack stackInCursorLast = ItemStack.EMPTY;

    public static void onSlotChangedCraftingGrid(World world, EntityPlayer player, IInventory craftMatrix, InventoryCraftResult inventoryCraftResult)
    {
        if (Configs.Generic.CLIENT_CRAFTING_FIX.getBooleanValue() &&
            world.isRemote && (world instanceof WorldClient) && player instanceof EntityPlayerSP)
        {
            ItemStack stack = ItemStack.EMPTY;
            IRecipe recipe = ((WorldClient) world).getRecipeManager().getRecipe(craftMatrix, world);

            if (recipe != null &&
                    (recipe.isDynamic() ||
                     world.getGameRules().getBoolean("doLimitedCrafting") == false ||
                     ((EntityPlayerSP) player).getRecipeBook().isUnlocked(recipe))
            )
            {
                inventoryCraftResult.setRecipeUsed(recipe);
                stack = recipe.getCraftingResult(craftMatrix);
            }

            inventoryCraftResult.setInventorySlotContents(0, stack);
        }
    }

    public static String getStackString(ItemStack stack)
    {
        if (isStackEmpty(stack) == false)
        {
            ResourceLocation rl = IRegistry.ITEM.getKey(stack.getItem());

            return String.format("[%s - display: %s - NBT: %s] (%s)",
                    rl != null ? rl.toString() : "null", stack.getDisplayName().getString(),
                    stack.getTag() != null ? stack.getTag().toString() : "<no NBT>",
                    stack.toString());
        }

        return "<empty>";
    }

    public static void debugPrintSlotInfo(GuiContainer gui, Slot slot)
    {
        if (slot == null)
        {
            ItemScroller.logger.info("slot was null");
            return;
        }

        boolean hasSlot = gui.inventorySlots.inventorySlots.contains(slot);
        Object inv = slot.inventory;
        String stackStr = InventoryUtils.getStackString(slot.getStack());

        ItemScroller.logger.info(String.format("slot: slotNumber: %d, getSlotIndex(): %d, getHasStack(): %s, " +
                "slot class: %s, inv class: %s, Container's slot list has slot: %s, stack: %s, numSlots: %d",
                slot.slotNumber, AccessorUtils.getSlotIndex(slot), slot.getHasStack(), slot.getClass().getName(),
                inv != null ? inv.getClass().getName() : "<null>", hasSlot ? " true" : "false", stackStr,
                gui.inventorySlots.inventorySlots.size()));
    }

    private static boolean isValidSlot(Slot slot, GuiContainer gui, boolean requireItems)
    {
        return gui.inventorySlots != null && gui.inventorySlots.inventorySlots != null &&
                slot != null && gui.inventorySlots.inventorySlots.contains(slot) &&
                (requireItems == false || slot.getHasStack()) &&
                Configs.SLOT_BLACKLIST.contains(slot.getClass().getName()) == false;
    }

    public static boolean isCraftingSlot(GuiContainer gui, @Nullable Slot slot)
    {
        return slot != null && CraftingHandler.getCraftingGridSlots(gui, slot) != null;
    }

    /**
     * Checks if there are slots belonging to another inventory on screen above the given slot
     */
    private static boolean inventoryExistsAbove(Slot slot, Container container)
    {
        for (Slot slotTmp : container.inventorySlots)
        {
            if (slotTmp.yPos < slot.yPos && areSlotsInSameInventory(slot, slotTmp) == false)
            {
                return true;
            }
        }

        return false;
    }

    public static boolean canShiftPlaceItems(GuiContainer gui)
    {
        Slot slot = AccessorUtils.getSlotUnderMouse(gui);
        Minecraft mc = Minecraft.getInstance();
        ItemStack stackCursor = mc.player.inventory.getItemStack();

        // The target slot needs to be an empty, valid slot, and there needs to be items in the cursor
        return slot != null && isStackEmpty(stackCursor) == false && isValidSlot(slot, gui, false) &&
               slot.getHasStack() == false && slot.isItemValid(stackCursor);
    }

    public static boolean tryMoveItems(GuiContainer gui, RecipeStorage recipes, boolean scrollingUp)
    {
        Slot slot = AccessorUtils.getSlotUnderMouse(gui);
        Minecraft mc = Minecraft.getInstance();

        // We require an empty cursor
        if (slot == null || isStackEmpty(mc.player.inventory.getItemStack()) == false)
        {
            return false;
        }

        // Villager handling only happens when scrolling over the trade output slot
        boolean villagerHandling = Configs.Toggles.SCROLL_VILLAGER.getBooleanValue() && gui instanceof GuiMerchant && slot instanceof SlotMerchantResult;
        boolean craftingHandling = Configs.Toggles.CRAFTING_FEATURES.getBooleanValue() && isCraftingSlot(gui, slot);
        boolean keyActiveMoveEverything = Hotkeys.MODIFIER_MOVE_EVERYTHING.getKeybind().isKeybindHeld();
        boolean keyActiveMoveMatching = Hotkeys.MODIFIER_MOVE_MATCHING.getKeybind().isKeybindHeld();
        boolean keyActiveMoveStacks = Hotkeys.MODIFIER_MOVE_STACK.getKeybind().isKeybindHeld();
        boolean nonSingleMove = keyActiveMoveEverything || keyActiveMoveMatching || keyActiveMoveStacks;
        boolean moveToOtherInventory = scrollingUp;

        if (Configs.Generic.SLOT_POSITION_AWARE_SCROLL_DIRECTION.getBooleanValue())
        {
            boolean above = inventoryExistsAbove(slot, gui.inventorySlots);
            // so basically: (above && scrollingUp) || (above == false && scrollingUp == false)
            moveToOtherInventory = (above == scrollingUp);
        }

        if ((Configs.Generic.REVERSE_SCROLL_DIRECTION_SINGLE.getBooleanValue() && nonSingleMove == false) ||
            (Configs.Generic.REVERSE_SCROLL_DIRECTION_STACKS.getBooleanValue() && nonSingleMove))
        {
            moveToOtherInventory = ! moveToOtherInventory;
        }

        // Check that the slot is valid, (don't require items in case of the villager output slot or a crafting slot)
        if (isValidSlot(slot, gui, villagerHandling || craftingHandling ? false : true) == false)
        {
            return false;
        }

        if (craftingHandling)
        {
            return tryMoveItemsCrafting(recipes, slot, gui, moveToOtherInventory, keyActiveMoveStacks, keyActiveMoveEverything);
        }

        if (villagerHandling)
        {
            return tryMoveItemsVillager((GuiMerchant) gui, slot, moveToOtherInventory, keyActiveMoveStacks);
        }

        if ((Configs.Toggles.SCROLL_SINGLE.getBooleanValue() == false && nonSingleMove == false) ||
            (Configs.Toggles.SCROLL_STACKS.getBooleanValue() == false && keyActiveMoveStacks) ||
            (Configs.Toggles.SCROLL_MATCHING.getBooleanValue() == false && keyActiveMoveMatching) ||
            (Configs.Toggles.SCROLL_EVERYTHING.getBooleanValue() == false && keyActiveMoveEverything))
        {
            return false;
        }

        // Move everything
        if (keyActiveMoveEverything)
        {
            tryMoveStacks(slot, gui, false, moveToOtherInventory, false);
        }
        // Move all matching items
        else if (keyActiveMoveMatching)
        {
            tryMoveStacks(slot, gui, true, moveToOtherInventory, false);
            return true;
        }
        // Move one matching stack
        else if (keyActiveMoveStacks)
        {
            tryMoveStacks(slot, gui, true, moveToOtherInventory, true);
        }
        else
        {
            ItemStack stack = slot.getStack();

            // Scrolling items from this slot/inventory into the other inventory
            if (moveToOtherInventory)
            {
                tryMoveSingleItemToOtherInventory(slot, gui);
            }
            // Scrolling items from the other inventory into this slot/inventory
            else if (getStackSize(stack) < slot.getItemStackLimit(stack))
            {
                tryMoveSingleItemToThisInventory(slot, gui);
            }
        }

        return false;
    }

    public static boolean dragMoveItems(GuiContainer gui, Minecraft mc, MoveAction action, int mouseX, int mouseY, boolean isClick)
    {
        if (isStackEmpty(mc.player.inventory.getItemStack()) == false)
        {
            // Updating these here is part of the fix to preventing a drag after shift + place
            lastPosX = mouseX;
            lastPosY = mouseY;
            stopDragging();

            return false;
        }

        boolean cancel = false;

        if (isClick && action != MoveAction.NONE)
        {
            // Reset this or the method call won't do anything...
            slotNumberLast = -1;
            lastPosX = mouseX;
            lastPosY = mouseY;
            activeMoveAction = action;
            cancel = dragMoveFromSlotAtPosition(gui, mouseX, mouseY, action);
        }
        else
        {
            action = activeMoveAction;
        }

        if (activeMoveAction != MoveAction.NONE && cancel == false)
        {
            int distX = mouseX - lastPosX;
            int distY = mouseY - lastPosY;
            int absX = Math.abs(distX);
            int absY = Math.abs(distY);

            if (absX > absY)
            {
                int inc = distX > 0 ? 1 : -1;

                for (int x = lastPosX; ; x += inc)
                {
                    int y = absX != 0 ? lastPosY + ((x - lastPosX) * distY / absX) : mouseY;
                    dragMoveFromSlotAtPosition(gui, x, y, action);

                    if (x == mouseX)
                    {
                        break;
                    }
                }
            }
            else
            {
                int inc = distY > 0 ? 1 : -1;

                for (int y = lastPosY; ; y += inc)
                {
                    int x = absY != 0 ? lastPosX + ((y - lastPosY) * distX / absY) : mouseX;
                    dragMoveFromSlotAtPosition(gui, x, y, action);

                    if (y == mouseY)
                    {
                        break;
                    }
                }
            }
        }

        lastPosX = mouseX;
        lastPosY = mouseY;

        // Always update the slot under the mouse.
        // This should prevent a "double click/move" when shift + left clicking on slots that have more
        // than one stack of items. (the regular slotClick() + a "drag move" from the slot that is under the mouse
        // when the left mouse button is pressed down and this code runs).
        Slot slot = AccessorUtils.getSlotAtPosition(gui, mouseX, mouseY);

        if (slot != null)
        {
            if (gui instanceof GuiContainerCreative)
            {
                boolean isPlayerInv = ((GuiContainerCreative) gui).getSelectedTabIndex() == ItemGroup.INVENTORY.getIndex();
                int slotNumber = isPlayerInv ? AccessorUtils.getSlotIndex(slot) : slot.slotNumber;
                slotNumberLast = slotNumber;
            }
            else
            {
                slotNumberLast = slot.slotNumber;
            }
        }
        else
        {
            slotNumberLast = -1;
        }

        return cancel;
    }

    public static void stopDragging()
    {
        activeMoveAction = MoveAction.NONE;
        DRAGGED_SLOTS.clear();
    }

    private static boolean dragMoveFromSlotAtPosition(GuiContainer gui, int x, int y, MoveAction action)
    {
        if (gui instanceof GuiContainerCreative)
        {
            return dragMoveFromSlotAtPositionCreative(gui, x, y, action);
        }

        Slot slot = AccessorUtils.getSlotAtPosition(gui, x, y);
        Minecraft mc = Minecraft.getInstance();
        MoveAmount amount = InputUtils.getMoveAmount(action);
        boolean flag = slot != null && isValidSlot(slot, gui, true) && slot.canTakeStack(mc.player);
        //boolean cancel = flag && (amount == MoveAmount.LEAVE_ONE || amount == MoveAmount.MOVE_ONE);

        if (flag && slot.slotNumber != slotNumberLast &&
            (amount != MoveAmount.MOVE_ONE || DRAGGED_SLOTS.contains(slot.slotNumber) == false))
        {
            switch (action)
            {
                case MOVE_TO_OTHER_MOVE_ONE:
                    tryMoveSingleItemToOtherInventory(slot, gui);
                    break;

                case MOVE_TO_OTHER_LEAVE_ONE:
                    tryMoveAllButOneItemToOtherInventory(slot, gui);
                    break;

                case MOVE_TO_OTHER_STACKS:
                    shiftClickSlot(gui, slot.slotNumber);
                    break;

                case MOVE_TO_OTHER_MATCHING:
                    tryMoveStacks(slot, gui, true, true, false);
                    break;

                case DROP_ONE:
                    clickSlot(gui, slot.slotNumber, 0, ClickType.THROW);
                    break;

                case DROP_LEAVE_ONE:
                    leftClickSlot(gui, slot.slotNumber);
                    rightClickSlot(gui, slot.slotNumber);
                    dropItemsFromCursor(gui);
                    break;

                case DROP_STACKS:
                    clickSlot(gui, slot.slotNumber, 1, ClickType.THROW);
                    break;

                case MOVE_DOWN_MOVE_ONE:
                case MOVE_DOWN_LEAVE_ONE:
                case MOVE_DOWN_STACKS:
                case MOVE_DOWN_MATCHING:
                    tryMoveItemsVertically(gui, slot, false, amount);
                    break;

                case MOVE_UP_MOVE_ONE:
                case MOVE_UP_LEAVE_ONE:
                case MOVE_UP_STACKS:
                case MOVE_UP_MATCHING:
                    tryMoveItemsVertically(gui, slot, true, amount);
                    break;

                default:
            }

            DRAGGED_SLOTS.add(slot.slotNumber);
        }

        return true;
    }

    private static boolean dragMoveFromSlotAtPositionCreative(GuiContainer gui, int x, int y, MoveAction action)
    {
        GuiContainerCreative guiCreative = (GuiContainerCreative) gui;
        Slot slot = AccessorUtils.getSlotAtPosition(gui, x, y);
        boolean isPlayerInv = guiCreative.getSelectedTabIndex() == ItemGroup.INVENTORY.getIndex();

        // Only allow dragging from the hotbar slots
        if (slot == null || (slot.getClass() != Slot.class && isPlayerInv == false))
        {
            return false;
        }

        Minecraft mc = Minecraft.getInstance();
        MoveAmount amount = InputUtils.getMoveAmount(action);
        boolean flag = slot != null && isValidSlot(slot, gui, true) && slot.canTakeStack(mc.player);
        boolean cancel = flag && (amount == MoveAmount.LEAVE_ONE || amount == MoveAmount.MOVE_ONE);
        // The player inventory tab of the creative inventory uses stupid wrapped
        // slots that all have slotNumber = 0 on the outer instance ;_;
        // However in that case we can use the slotIndex which is easy enough to get.
        int slotNumber = isPlayerInv ? AccessorUtils.getSlotIndex(slot) : slot.slotNumber;

        if (flag && slotNumber != slotNumberLast && DRAGGED_SLOTS.contains(slotNumber) == false)
        {
            switch (action)
            {
                case SCROLL_TO_OTHER_MOVE_ONE:
                case MOVE_TO_OTHER_MOVE_ONE:
                    leftClickSlot(guiCreative, slot, slotNumber);
                    rightClickSlot(guiCreative, slot, slotNumber);
                    shiftClickSlot(guiCreative, slot, slotNumber);
                    leftClickSlot(guiCreative, slot, slotNumber);

                    cancel = true;
                    break;

                case MOVE_TO_OTHER_LEAVE_ONE:
                    // Too lazy to try to duplicate the proper code for the weird creative inventory...
                    if (isPlayerInv == false)
                    {
                        leftClickSlot(guiCreative, slot, slotNumber);
                        rightClickSlot(guiCreative, slot, slotNumber);

                        // Delete the rest of the stack by placing it in the first creative "source slot"
                        Slot slotFirst = gui.inventorySlots.inventorySlots.get(0);
                        leftClickSlot(guiCreative, slotFirst, slotFirst.slotNumber);
                    }

                    cancel = true;
                    break;

                case SCROLL_TO_OTHER_STACKS:
                case MOVE_TO_OTHER_STACKS:
                    shiftClickSlot(gui, slot, slotNumber);
                    cancel = true;
                    break;

                case DROP_ONE:
                    clickSlot(gui, slot.slotNumber, 0, ClickType.THROW);
                    break;

                case DROP_LEAVE_ONE:
                    leftClickSlot(gui, slot.slotNumber);
                    rightClickSlot(gui, slot.slotNumber);
                    dropItemsFromCursor(gui);
                    break;

                case DROP_STACKS:
                    clickSlot(gui, slot.slotNumber, 1, ClickType.THROW);
                    cancel = true;
                    break;

                case MOVE_DOWN_MOVE_ONE:
                case MOVE_DOWN_LEAVE_ONE:
                case MOVE_DOWN_STACKS:
                    tryMoveItemsVertically(gui, slot, false, amount);
                    cancel = true;
                    break;

                case MOVE_UP_MOVE_ONE:
                case MOVE_UP_LEAVE_ONE:
                case MOVE_UP_STACKS:
                    tryMoveItemsVertically(gui, slot, true, amount);
                    cancel = true;
                    break;

                default:
            }

            DRAGGED_SLOTS.add(slotNumber);
        }

        return cancel;
    }

    public static void dropStacks(GuiContainer gui, ItemStack stackReference, Slot slotReference, boolean sameInventory)
    {
        if (slotReference != null && isStackEmpty(stackReference) == false)
        {
            Container container = gui.inventorySlots;
            stackReference = stackReference.copy();

            for (Slot slot : container.inventorySlots)
            {
                // If this slot is in the same inventory that the items were picked up to the cursor from
                // and the stack is identical to the one in the cursor, then this stack will get dropped.
                if (areSlotsInSameInventory(slot, slotReference) == sameInventory && areStacksEqual(slot.getStack(), stackReference))
                {
                    // Drop the stack
                    dropStack(gui, slot.slotNumber);
                }
            }
        }
    }

    public static boolean shiftDropItems(GuiContainer gui)
    {
        ItemStack stackReference = Minecraft.getInstance().player.inventory.getItemStack();

        if (isStackEmpty(stackReference) == false && sourceSlot != null)
        {
            stackReference = stackReference.copy();

            // First drop the existing stack from the cursor
            dropItemsFromCursor(gui);

            dropStacks(gui, stackReference, sourceSlot.get(), true);
            return true;
        }

        return false;
    }

    public static boolean shiftPlaceItems(Slot slot, GuiContainer gui)
    {
        // Left click to place the items from the cursor to the slot
        leftClickSlot(gui, slot.slotNumber);

        // Ugly fix to prevent accidentally drag-moving the stack from the slot that it was just placed into...
        DRAGGED_SLOTS.add(slot.slotNumber);

        tryMoveStacks(slot, gui, true, false, false);

        return true;
    }

    /**
     * Store a reference to the slot when a slot is left or right clicked on.
     * The slot is then later used to determine which inventory an ItemStack was
     * picked up from, if the stack from the cursor is dropped while holding shift.
     */
    public static void storeSourceSlotCandidate(Slot slot, Minecraft mc)
    {
        // Left or right mouse button was pressed
        if (slot != null)
        {
            ItemStack stackCursor = mc.player.inventory.getItemStack();
            ItemStack stack = EMPTY_STACK;

            if (isStackEmpty(stackCursor) == false)
            {
                // Do a cheap copy without NBT data
                stack = new ItemStack(stackCursor.getItem(), getStackSize(stackCursor));
            }

            // Store the candidate
            // NOTE: This method is called BEFORE the stack has been picked up to the cursor!
            // Thus we can't check that there is an item already in the cursor, and that's why this is just a "candidate"
            sourceSlotCandidate = new WeakReference<>(slot);
            stackInCursorLast = stack;
        }
    }

    /**
     * Check if the (previous) mouse event resulted in picking up a new ItemStack to the cursor
     */
    public static void checkForItemPickup(GuiContainer gui, Minecraft mc)
    {
        ItemStack stackCursor = mc.player.inventory.getItemStack();

        // Picked up or swapped items to the cursor, grab a reference to the slot that the items came from
        // Note that we are only checking the item and metadata here!
        if (isStackEmpty(stackCursor) == false && stackCursor.isItemEqual(stackInCursorLast) == false && sourceSlotCandidate != null)
        {
            sourceSlot = new WeakReference<>(sourceSlotCandidate.get());
        }
    }

    private static boolean tryMoveItemsVillager(GuiMerchant gui, Slot slot, boolean moveToOtherInventory, boolean fullStacks)
    {
        if (fullStacks)
        {
            // Try to fill the merchant's buy slots from the player inventory
            if (moveToOtherInventory == false)
            {
                tryMoveItemsToMerchantBuySlots(gui, true);
            }
            // Move items from sell slot to player inventory
            else if (slot.getHasStack())
            {
                tryMoveStacks(slot, gui, true, true, true);
            }
            // Scrolling over an empty output slot, clear the buy slots
            else
            {
                tryMoveStacks(slot, gui, false, true, false);
            }
        }
        else
        {
            // Scrolling items from player inventory into merchant buy slots
            if (moveToOtherInventory == false)
            {
                tryMoveItemsToMerchantBuySlots(gui, false);
            }
            // Scrolling items from this slot/inventory into the other inventory
            else if (slot.getHasStack())
            {
                moveOneSetOfItemsFromSlotToPlayerInventory(gui, slot);
            }
        }

        return false;
    }

    public static void villagerClearTradeInputSlots()
    {
        if (GuiUtils.getCurrentScreen() instanceof GuiMerchant)
        {
            GuiMerchant merchantGui = (GuiMerchant) GuiUtils.getCurrentScreen();
            Slot slot = merchantGui.inventorySlots.getSlot(0);

            if (slot.getHasStack())
            {
                shiftClickSlot(merchantGui, slot.slotNumber);
            }

            slot = merchantGui.inventorySlots.getSlot(1);

            if (slot.getHasStack())
            {
                InventoryUtils.shiftClickSlot(merchantGui, slot.slotNumber);
            }
        }
    }

    public static void villagerTradeEverythingPossibleWithCurrentRecipe()
    {
        if (GuiUtils.getCurrentScreen() instanceof GuiMerchant)
        {
            GuiMerchant merchantGui = (GuiMerchant) GuiUtils.getCurrentScreen();
            Slot slot = merchantGui.inventorySlots.getSlot(2);

            while (true)
            {
                InventoryUtils.tryMoveItemsToMerchantBuySlots(merchantGui, true);

                // Not a valid recipe
                if (slot.getHasStack() == false)
                {
                    break;
                }

                InventoryUtils.shiftClickSlot(merchantGui, slot.slotNumber);

                // No room in player inventory
                if (slot.getHasStack())
                {
                    break;
                }
            }

            villagerClearTradeInputSlots();
        }
    }

    public static void villagerTradeEverythingPossibleWithAllFavoritedTrades()
    {
        if (GuiUtils.getCurrentScreen() instanceof GuiMerchant)
        {
            GuiMerchant merchantGui = (GuiMerchant) GuiUtils.getCurrentScreen();
            VillagerData data = VillagerDataStorage.getInstance().getDataForLastInteractionTarget();

            villagerClearTradeInputSlots();

            if (data != null && data.getFavorites().isEmpty() == false)
            {
                int initialPage = AccessorUtils.getSelectedMerchantRecipe(merchantGui);

                for (int index : data.getFavorites())
                {
                    InputHandler.changeTradePage(merchantGui, index);
                    villagerTradeEverythingPossibleWithCurrentRecipe();
                }

                InputHandler.changeTradePage(merchantGui, initialPage);
            }
        }
    }

    private static boolean tryMoveSingleItemToOtherInventory(Slot slot, GuiContainer gui)
    {
        ItemStack stackOrig = slot.getStack();
        Container container = gui.inventorySlots;
        Minecraft mc = Minecraft.getInstance();

        if (isStackEmpty(mc.player.inventory.getItemStack()) == false || slot.canTakeStack(mc.player) == false ||
            (getStackSize(stackOrig) > 1 && slot.isItemValid(stackOrig) == false))
        {
            return false;
        }

        // Can take all the items to the cursor at once, use a shift-click method to move one item from the slot
        if (getStackSize(stackOrig) <= stackOrig.getMaxStackSize())
        {
            return clickSlotsToMoveSingleItemByShiftClick(gui, slot.slotNumber);
        }

        ItemStack stack = stackOrig.copy();
        setStackSize(stack, 1);

        ItemStack[] originalStacks = getOriginalStacks(container);

        // Try to move the temporary single-item stack via the shift-click handler method
        slot.putStack(stack);
        container.transferStackInSlot(mc.player, slot.slotNumber);

        // Successfully moved the item somewhere, now we want to check where it went
        if (slot.getHasStack() == false)
        {
            int targetSlot = getTargetSlot(container, originalStacks);

            // Found where the item went
            if (targetSlot >= 0)
            {
                // Remove the dummy item from the target slot (on the client side)
                container.inventorySlots.get(targetSlot).decrStackSize(1);

                // Restore the original stack to the slot under the cursor (on the client side)
                restoreOriginalStacks(container, originalStacks);

                // Do the slot clicks to actually move the items (on the server side)
                return clickSlotsToMoveSingleItem(gui, slot.slotNumber, targetSlot);
            }
        }

        // Restore the original stack to the slot under the cursor (on the client side)
        slot.putStack(stackOrig);

        return false;
    }

    private static boolean tryMoveAllButOneItemToOtherInventory(Slot slot, GuiContainer gui)
    {
        Minecraft mc = Minecraft.getInstance();
        EntityPlayer player = mc.player;
        ItemStack stackOrig = slot.getStack().copy();

        if (getStackSize(stackOrig) == 1 || getStackSize(stackOrig) > stackOrig.getMaxStackSize() ||
            slot.canTakeStack(player) == false || slot.isItemValid(stackOrig) == false)
        {
            return true;
        }

        // Take half of the items from the original slot to the cursor
        rightClickSlot(gui, slot.slotNumber);

        ItemStack stackInCursor = player.inventory.getItemStack();
        if (isStackEmpty(stackInCursor))
        {
            return false;
        }

        int stackInCursorSizeOrig = getStackSize(stackInCursor);
        int tempSlotNum = -1;

        // Find some other slot where to store one of the items temporarily
        for (Slot slotTmp : gui.inventorySlots.inventorySlots)
        {
            if (slotTmp.slotNumber != slot.slotNumber &&
                areSlotsInSameInventory(slotTmp, slot, true) &&
                slotTmp.isItemValid(stackInCursor) &&
                slotTmp.canTakeStack(player))
            {
                ItemStack stackInSlot = slotTmp.getStack();

                if (isStackEmpty(stackInSlot) || areStacksEqual(stackInSlot, stackInCursor))
                {
                    // Try to put one item into the temporary slot
                    rightClickSlot(gui, slotTmp.slotNumber);

                    stackInCursor = player.inventory.getItemStack();

                    // Successfully stored one item
                    if (isStackEmpty(stackInCursor) || getStackSize(stackInCursor) < stackInCursorSizeOrig)
                    {
                        tempSlotNum = slotTmp.slotNumber;
                        break;
                    }
                }
            }
        }

        if (isStackEmpty(player.inventory.getItemStack()) == false)
        {
            // Return the rest of the items into the original slot
            leftClickSlot(gui, slot.slotNumber);
        }

        // Successfully stored one item in a temporary slot
        if (tempSlotNum != -1)
        {
            // Shift click the stack from the original slot
            shiftClickSlot(gui, slot.slotNumber);

            // Take half a stack from the temporary slot
            rightClickSlot(gui, tempSlotNum);

            // Return one item into the original slot
            rightClickSlot(gui, slot.slotNumber);

            // Return the rest of the items to the temporary slot, if any
            if (isStackEmpty(player.inventory.getItemStack()) == false)
            {
                leftClickSlot(gui, tempSlotNum);
            }

            return true;
        }
        // No temporary slot found, try to move the stack manually
        else
        {
            boolean treatHotbarAsDifferent = gui.getClass() == GuiInventory.class;
            List<Integer> slots = getSlotNumbersOfEmptySlots(gui.inventorySlots, slot, false, treatHotbarAsDifferent, false);

            if (slots.isEmpty())
            {
                slots = getSlotNumbersOfMatchingStacks(gui.inventorySlots, slot, false, slot.getStack(), true, treatHotbarAsDifferent, false);
            }

            if (slots.isEmpty() == false)
            {
                // Take the stack
                leftClickSlot(gui, slot.slotNumber);

                // Return one item
                rightClickSlot(gui, slot.slotNumber);

                // Try to place the stack in the cursor to any valid empty or matching slots in a different inventory
                for (int slotNum : slots)
                {
                    Slot slotTmp = gui.inventorySlots.getSlot(slotNum);
                    stackInCursor = player.inventory.getItemStack();

                    if (isStackEmpty(stackInCursor))
                    {
                        return true;
                    }

                    if (slotTmp.isItemValid(stackInCursor))
                    {
                        leftClickSlot(gui, slotNum);
                    }
                }

                // Items left, return them
                if (isStackEmpty(stackInCursor) == false)
                {
                    leftClickSlot(gui, slot.slotNumber);
                }
            }
        }

        return false;
    }

    private static boolean tryMoveSingleItemToThisInventory(Slot slot, GuiContainer gui)
    {
        Container container = gui.inventorySlots;
        ItemStack stackOrig = slot.getStack();
        Minecraft mc = Minecraft.getInstance();

        if (slot.isItemValid(stackOrig) == false)
        {
            return false;
        }

        for (int slotNum = container.inventorySlots.size() - 1; slotNum >= 0; slotNum--)
        {
            Slot slotTmp = container.inventorySlots.get(slotNum);
            ItemStack stackTmp = slotTmp.getStack();

            if (areSlotsInSameInventory(slotTmp, slot) == false &&
                isStackEmpty(stackTmp) == false && slotTmp.canTakeStack(mc.player) &&
                (getStackSize(stackTmp) == 1 || slotTmp.isItemValid(stackTmp)))
            {
                if (areStacksEqual(stackTmp, stackOrig))
                {
                    return clickSlotsToMoveSingleItem(gui, slotTmp.slotNumber, slot.slotNumber);
                }
            }
        }

        // If we weren't able to move any items from another inventory, then try to move items
        // within the same inventory (mostly between the hotbar and the player inventory)
        /*
        for (Slot slotTmp : container.inventorySlots)
        {
            ItemStack stackTmp = slotTmp.getStack();

            if (slotTmp.slotNumber != slot.slotNumber &&
                isStackEmpty(stackTmp) == false && slotTmp.canTakeStack(gui.mc.player) &&
                (getStackSize(stackTmp) == 1 || slotTmp.isItemValid(stackTmp)))
            {
                if (areStacksEqual(stackTmp, stackOrig))
                {
                    return this.clickSlotsToMoveSingleItem(gui, slotTmp.slotNumber, slot.slotNumber);
                }
            }
        }
        */

        return false;
    }

    public static void tryMoveStacks(Slot slot, GuiContainer gui, boolean matchingOnly, boolean toOtherInventory, boolean firstOnly)
    {
        tryMoveStacks(slot.getStack(), slot, gui, matchingOnly, toOtherInventory, firstOnly);
    }

    private static void tryMoveStacks(ItemStack stackReference, Slot slot, GuiContainer gui, boolean matchingOnly, boolean toOtherInventory, boolean firstOnly)
    {
        Container container = gui.inventorySlots;
        final int maxSlot = container.inventorySlots.size() - 1;

        for (int i = maxSlot; i >= 0; i--)
        {
            Slot slotTmp = container.inventorySlots.get(i);

            if (slotTmp.slotNumber != slot.slotNumber &&
                areSlotsInSameInventory(slotTmp, slot) == toOtherInventory && slotTmp.getHasStack() &&
                (matchingOnly == false || areStacksEqual(stackReference, slotTmp.getStack())))
            {
                boolean success = shiftClickSlotWithCheck(gui, slotTmp.slotNumber);

                // Failed to shift-click items, try a manual method
                if (success == false && Configs.Toggles.SCROLL_STACKS_FALLBACK.getBooleanValue())
                {
                    clickSlotsToMoveItemsFromSlot(slotTmp, gui, toOtherInventory);
                }

                if (firstOnly)
                {
                    return;
                }
            }
        }

        // If moving to the other inventory, then move the hovered slot's stack last
        if (toOtherInventory && shiftClickSlotWithCheck(gui, slot.slotNumber) == false)
        {
            clickSlotsToMoveItemsFromSlot(slot, gui, toOtherInventory);
        }
    }

    public static void tryMoveItemsToMerchantBuySlots(GuiMerchant gui, boolean fillStacks)
    {
        Minecraft mc = Minecraft.getInstance();
        MerchantRecipeList list = gui.getMerchant().getRecipes(mc.player);
        int index = AccessorUtils.getSelectedMerchantRecipe(gui);

        if (list == null || list.size() <= index)
        {
            return;
        }

        MerchantRecipe recipe = list.get(index);
        if (recipe == null)
        {
            return;
        }

        ItemStack buy1 = recipe.getItemToBuy();
        ItemStack buy2 = recipe.getSecondItemToBuy();

        if (isStackEmpty(buy1) == false)
        {
            fillBuySlot(gui, 0, buy1, fillStacks);
        }

        if (isStackEmpty(buy2) == false)
        {
            fillBuySlot(gui, 1, buy2, fillStacks);
        }
    }

    private static void fillBuySlot(GuiContainer gui, int slotNum, ItemStack buyStack, boolean fillStacks)
    {
        Slot slot = gui.inventorySlots.getSlot(slotNum);
        ItemStack existingStack = slot.getStack();
        Minecraft mc = Minecraft.getInstance();

        // If there are items not matching the merchant recipe, move them out first
        if (isStackEmpty(existingStack) == false && areStacksEqual(buyStack, existingStack) == false)
        {
            shiftClickSlot(gui, slotNum);
        }

        existingStack = slot.getStack();

        if (isStackEmpty(existingStack) || areStacksEqual(buyStack, existingStack))
        {
            moveItemsFromInventory(gui, slotNum, mc.player.inventory, buyStack, fillStacks);
        }
    }

    public static void handleRecipeClick(GuiContainer gui, Minecraft mc, RecipeStorage recipes, int hoveredRecipeId,
            boolean isLeftClick, boolean isRightClick, boolean isPickBlock, boolean isShiftDown)
    {
        if (isLeftClick || isRightClick)
        {
            boolean changed = recipes.getSelection() != hoveredRecipeId;
            recipes.changeSelectedRecipe(hoveredRecipeId);

            if (changed)
            {
                InventoryUtils.clearFirstCraftingGridOfItems(recipes.getSelectedRecipe(), gui, false);
            }
            else
            {
                InventoryUtils.tryMoveItemsToFirstCraftingGrid(recipes.getRecipe(hoveredRecipeId), gui, isShiftDown);
            }

            // Right click: Also craft the items
            if (isRightClick)
            {
                Slot outputSlot = CraftingHandler.getFirstCraftingOutputSlotForGui(gui);
                boolean dropKeyDown = mc.gameSettings.keyBindDrop.isKeyDown();

                if (outputSlot != null)
                {
                    if (dropKeyDown)
                    {
                        if (isShiftDown)
                        {
                            if (Configs.Generic.CARPET_CTRL_Q_CRAFTING.getBooleanValue())
                            {
                                InventoryUtils.dropStack(gui, outputSlot.slotNumber);
                            }
                            else
                            {
                                InventoryUtils.dropStacksUntilEmpty(gui, outputSlot.slotNumber);
                            }
                        }
                        else
                        {
                            InventoryUtils.dropItem(gui, outputSlot.slotNumber);
                        }
                    }
                    else
                    {
                        if (isShiftDown)
                        {
                            InventoryUtils.shiftClickSlot(gui, outputSlot.slotNumber);
                        }
                        else
                        {
                            InventoryUtils.moveOneSetOfItemsFromSlotToPlayerInventory(gui, outputSlot);
                        }
                    }
                }
            }
        }
        else if (isPickBlock)
        {
            InventoryUtils.clearFirstCraftingGridOfAllItems(gui);
        }
    }

    public static void tryMoveItemsToFirstCraftingGrid(CraftingRecipe recipe, GuiContainer gui, boolean fillStacks)
    {
        Slot craftingOutputSlot = CraftingHandler.getFirstCraftingOutputSlotForGui(gui);

        if (craftingOutputSlot != null)
        {
            tryMoveItemsToCraftingGridSlots(recipe, craftingOutputSlot, gui, fillStacks);
        }
    }

    public static void loadRecipeItemsToGridForOutputSlotUnderMouse(CraftingRecipe recipe, GuiContainer gui)
    {
        Slot slot = AccessorUtils.getSlotUnderMouse(gui);
        loadRecipeItemsToGridForOutputSlot(recipe, gui, slot);
    }

    private static void loadRecipeItemsToGridForOutputSlot(CraftingRecipe recipe, GuiContainer gui, Slot outputSlot)
    {
        if (outputSlot != null && isCraftingSlot(gui, outputSlot) && isStackEmpty(recipe.getResult()) == false)
        {
            tryMoveItemsToCraftingGridSlots(recipe, outputSlot, gui, false);
        }
    }

    private static boolean tryMoveItemsCrafting(RecipeStorage recipes, Slot slot, GuiContainer gui,
            boolean moveToOtherInventory, boolean moveStacks, boolean moveEverything)
    {
        CraftingRecipe recipe = recipes.getSelectedRecipe();
        ItemStack stackRecipeOutput = recipe.getResult();

        // Try to craft items
        if (moveToOtherInventory)
        {
            // Items in the output slot
            if (slot.getHasStack())
            {
                // The output item matches the current recipe
                if (areStacksEqual(slot.getStack(), stackRecipeOutput))
                {
                    if (moveEverything)
                    {
                        craftAsManyItemsAsPossible(recipe, slot, gui);
                    }
                    else if (moveStacks)
                    {
                        shiftClickSlot(gui, slot.slotNumber);
                    }
                    else
                    {
                        moveOneSetOfItemsFromSlotToPlayerInventory(gui, slot);
                    }
                }
            }
            // Scrolling over an empty output slot, clear the grid
            else
            {
                clearCraftingGridOfAllItems(gui, CraftingHandler.getCraftingGridSlots(gui, slot));
            }
        }
        // Try to move items to the grid
        else if (moveToOtherInventory == false && isStackEmpty(stackRecipeOutput) == false)
        {
            tryMoveItemsToCraftingGridSlots(recipe, slot, gui, moveStacks);
        }

        return false;
    }

    private static void craftAsManyItemsAsPossible(CraftingRecipe recipe, Slot slot, GuiContainer gui)
    {
        ItemStack result = recipe.getResult();
        int failSafe = 1024;

        while (failSafe > 0 && slot.getHasStack() && areStacksEqual(slot.getStack(), result))
        {
            shiftClickSlot(gui, slot.slotNumber);

            // Ran out of some or all ingredients for the recipe
            if (slot.getHasStack() == false || areStacksEqual(slot.getStack(), result) == false)
            {
                tryMoveItemsToCraftingGridSlots(recipe, slot, gui, true);
            }
            // No change in the result slot after shift clicking, let's assume the craft failed and stop here
            else
            {
                break;
            }

            failSafe--;
        }
    }

    public static void clearFirstCraftingGridOfItems(CraftingRecipe recipe, GuiContainer gui, boolean clearNonMatchingOnly)
    {
        Slot craftingOutputSlot = CraftingHandler.getFirstCraftingOutputSlotForGui(gui);

        if (craftingOutputSlot != null)
        {
            SlotRange range = CraftingHandler.getCraftingGridSlots(gui, craftingOutputSlot);
            clearCraftingGridOfItems(recipe, gui, range, clearNonMatchingOnly);
        }
    }

    public static void clearFirstCraftingGridOfAllItems(GuiContainer gui)
    {
        Slot craftingOutputSlot = CraftingHandler.getFirstCraftingOutputSlotForGui(gui);

        if (craftingOutputSlot != null)
        {
            SlotRange range = CraftingHandler.getCraftingGridSlots(gui, craftingOutputSlot);
            clearCraftingGridOfAllItems(gui, range);
        }
    }

    private static boolean clearCraftingGridOfItems(CraftingRecipe recipe, GuiContainer gui, SlotRange range, boolean clearNonMatchingOnly)
    {
        final int invSlots = gui.inventorySlots.inventorySlots.size();
        final int rangeSlots = range.getSlotCount();
        final int recipeSize = recipe.getRecipeLength();
        final int slotCount = Math.min(rangeSlots, recipeSize);

        for (int i = 0, slotNum = range.getFirst(); i < slotCount && slotNum < invSlots; i++, slotNum++)
        {
            Slot slotTmp = gui.inventorySlots.getSlot(slotNum);

            if (slotTmp != null && slotTmp.getHasStack() &&
                (clearNonMatchingOnly == false || areStacksEqual(recipe.getRecipeItems()[i], slotTmp.getStack()) == false))
            {
                shiftClickSlot(gui, slotNum);

                // Failed to clear the slot
                if (slotTmp.getHasStack())
                {
                    return false;
                }
            }
        }

        return true;
    }

    private static boolean clearCraftingGridOfAllItems(GuiContainer gui, SlotRange range)
    {
        final int invSlots = gui.inventorySlots.inventorySlots.size();
        final int rangeSlots = range.getSlotCount();
        boolean clearedAll = true;

        for (int i = 0, slotNum = range.getFirst(); i < rangeSlots && slotNum < invSlots; i++, slotNum++)
        {
            Slot slotTmp = gui.inventorySlots.getSlot(slotNum);

            if (slotTmp != null && slotTmp.getHasStack())
            {
                shiftClickSlot(gui, slotNum);

                // Failed to clear the slot
                if (slotTmp.getHasStack())
                {
                    clearedAll = false;
                }
            }
        }

        return clearedAll;
    }

    private static boolean tryMoveItemsToCraftingGridSlots(CraftingRecipe recipe, Slot slot, GuiContainer gui, boolean fillStacks)
    {
        Container container = gui.inventorySlots;
        int numSlots = container.inventorySlots.size();
        SlotRange range = CraftingHandler.getCraftingGridSlots(gui, slot);

        // Check that the slot range is valid and that the recipe can fit into this type of crafting grid
        if (range != null && range.getLast() < numSlots && recipe.getRecipeLength() <= range.getSlotCount())
        {
            // Clear non-matching items from the grid first
            if (clearCraftingGridOfItems(recipe, gui, range, true) == false)
            {
                return false;
            }

            // This slot is used to check that we get items from a DIFFERENT inventory than where this slot is in
            Slot slotGridFirst = container.getSlot(range.getFirst());
            Map<ItemType, List<Integer>> ingredientSlots = ItemType.getSlotsPerItem(recipe.getRecipeItems());

            for (Map.Entry<ItemType, List<Integer>> entry : ingredientSlots.entrySet())
            {
                ItemStack ingredientReference = entry.getKey().getStack();
                List<Integer> recipeSlots = entry.getValue();
                List<Integer> targetSlots = new ArrayList<Integer>();

                // Get the actual target slot numbers based on the grid's start and the relative positions inside the grid
                for (int s : recipeSlots)
                {
                    targetSlots.add(s + range.getFirst());
                }

                if (fillStacks)
                {
                    fillCraftingGrid(gui, slotGridFirst, ingredientReference, targetSlots);
                }
                else
                {
                    moveOneRecipeItemIntoCraftingGrid(gui, slotGridFirst, ingredientReference, targetSlots);
                }
            }
        }

        return false;
    }

    private static void fillCraftingGrid(GuiContainer gui, Slot slotGridFirst, ItemStack ingredientReference, List<Integer> targetSlots)
    {
        Container container = gui.inventorySlots;
        Minecraft mc = Minecraft.getInstance();
        EntityPlayer player = mc.player;
        int slotNum = -1;
        int slotReturn = -1;
        int sizeOrig = 0;

        if (isStackEmpty(ingredientReference))
        {
            return;
        }

        while (true)
        {
            slotNum = getSlotNumberOfLargestMatchingStackFromDifferentInventory(container, slotGridFirst, ingredientReference);

            // Didn't find ingredient items
            if (slotNum < 0)
            {
                break;
            }

            if (slotReturn == -1)
            {
                slotReturn = slotNum;
            }

            // Pick up the ingredient stack from the found slot
            leftClickSlot(gui, slotNum);

            ItemStack stackCursor = player.inventory.getItemStack();

            // Successfully picked up ingredient items
            if (areStacksEqual(ingredientReference, stackCursor))
            {
                sizeOrig = getStackSize(stackCursor);
                dragSplitItemsIntoSlots(gui, targetSlots);
                stackCursor = player.inventory.getItemStack();

                // Items left in cursor
                if (isStackEmpty(stackCursor) == false)
                {
                    // Didn't manage to move any items anymore
                    if (getStackSize(stackCursor) >= sizeOrig)
                    {
                        break;
                    }

                    // Collect all the remaining items into the first found slot, as long as possible
                    leftClickSlot(gui, slotReturn);

                    // All of them didn't fit into the first slot anymore, switch into the current source slot
                    if (isStackEmpty(player.inventory.getItemStack()) == false)
                    {
                        slotReturn = slotNum;
                        leftClickSlot(gui, slotReturn);
                    }
                }
            }
            // Failed to pick up the stack, break to avoid infinite loops
            // TODO: we could also "blacklist" this slot and try to continue...?
            else
            {
                break;
            }

            // Somehow items were left in the cursor, break here
            if (isStackEmpty(player.inventory.getItemStack()) == false)
            {
                break;
            }
        }

        // Return the rest of the items to the original slot
        if (slotNum >= 0 && isStackEmpty(player.inventory.getItemStack()) == false)
        {
            leftClickSlot(gui, slotNum);
        }
    }

    public static void rightClickCraftOneStack(GuiContainer gui)
    {
        Slot slot = AccessorUtils.getSlotUnderMouse(gui);
        Minecraft mc = Minecraft.getInstance();
        InventoryPlayer inv = mc.player.inventory;
        ItemStack stackCursor = inv.getItemStack();

        if (slot == null || slot.getHasStack() == false ||
            (isStackEmpty(stackCursor) == false) && areStacksEqual(slot.getStack(), stackCursor) == false)
        {
            return;
        }

        int sizeLast = 0;

        while (true)
        {
            rightClickSlot(gui, slot.slotNumber);
            stackCursor = inv.getItemStack();

            // Failed to craft items, or the stack became full, or ran out of ingredients
            if (isStackEmpty(stackCursor) || getStackSize(stackCursor) <= sizeLast ||
                getStackSize(stackCursor) >= stackCursor.getMaxStackSize() ||
                areStacksEqual(slot.getStack(), stackCursor) == false)
            {
                break;
            }

            sizeLast = getStackSize(stackCursor);
        }
    }

    public static void craftEverythingPossibleWithCurrentRecipe(CraftingRecipe recipe, GuiContainer gui)
    {
        Slot slot = CraftingHandler.getFirstCraftingOutputSlotForGui(gui);

        if (slot != null && isStackEmpty(recipe.getResult()) == false)
        {
            SlotRange range = CraftingHandler.getCraftingGridSlots(gui, slot);

            if (range != null)
            {
                // Clear all items from the grid first, to avoid unbalanced stacks
                if (clearCraftingGridOfItems(recipe, gui, range, false) == false)
                {
                    return;
                }

                tryMoveItemsToCraftingGridSlots(recipe, slot, gui, true);

                if (slot.getHasStack())
                {
                    craftAsManyItemsAsPossible(recipe, slot, gui);
                }
            }
        }
    }

    public static void moveAllCraftingResultsToOtherInventory(CraftingRecipe recipe, GuiContainer gui)
    {
        if (isStackEmpty(recipe.getResult()) == false)
        {
            Slot slot = null;
            ItemStack stackResult = recipe.getResult().copy();

            for (Slot slotTmp : gui.inventorySlots.inventorySlots)
            {
                // This slot is likely in the player inventory, as there is another inventory above
                if (areStacksEqual(slotTmp.getStack(), stackResult) &&
                    inventoryExistsAbove(slotTmp, gui.inventorySlots))
                {
                    slot = slotTmp;
                    break;
                }
            }

            if (slot != null)
            {
                // Get a list of slots with matching items, which are in the same inventory
                // as the slot that is assumed to be in the player inventory.
                List<Integer> slots = getSlotNumbersOfMatchingStacks(gui.inventorySlots, slot, true, stackResult, false, false, false);

                for (int slotNum : slots)
                {
                    shiftClickSlot(gui, slotNum);
                }
            }
        }
    }

    public static void throwAllCraftingResultsToGround(CraftingRecipe recipe, GuiContainer gui)
    {
        Slot slot = CraftingHandler.getFirstCraftingOutputSlotForGui(gui);

        if (slot != null && isStackEmpty(recipe.getResult()) == false)
        {
            dropStacks(gui, recipe.getResult(), slot, false);
        }
    }

    private static int putSingleItemIntoSlots(GuiContainer gui, List<Integer> targetSlots, int startIndex)
    {
        Minecraft mc = Minecraft.getInstance();
        ItemStack stackInCursor = mc.player.inventory.getItemStack();

        if (isStackEmpty(stackInCursor))
        {
            return 0;
        }

        int numSlots = gui.inventorySlots.inventorySlots.size();
        int numItems = getStackSize(stackInCursor);
        int loops = Math.min(numItems, targetSlots.size() - startIndex);
        int count = 0;

        for (int i = 0; i < loops; i++)
        {
            int slotNum = targetSlots.get(startIndex + i);

            if (slotNum >= numSlots)
            {
                break;
            }

            rightClickSlot(gui, slotNum);
            count++;
        }

        return count;
    }

    public static void moveOneSetOfItemsFromSlotToPlayerInventory(GuiContainer gui, Slot slot)
    {
        leftClickSlot(gui, slot.slotNumber);

        Minecraft mc = Minecraft.getInstance();
        ItemStack stackCursor = mc.player.inventory.getItemStack();

        if (isStackEmpty(stackCursor) == false)
        {
            List<Integer> slots = getSlotNumbersOfMatchingStacks(gui.inventorySlots, slot, false, stackCursor, true, true, false);

            if (moveItemFromCursorToSlots(gui, slots) == false)
            {
                slots = getSlotNumbersOfEmptySlotsInPlayerInventory(gui.inventorySlots, false);
                moveItemFromCursorToSlots(gui, slots);
            }
        }
    }

    private static void moveOneRecipeItemIntoCraftingGrid(GuiContainer gui, Slot slotGridFirst, ItemStack ingredientReference, List<Integer> targetSlots)
    {
        Container container = gui.inventorySlots;
        Minecraft mc = Minecraft.getInstance();
        int index = 0;
        int slotNum = -1;
        int slotCount = targetSlots.size();

        while (index < slotCount)
        {
            slotNum = getSlotNumberOfSmallestStackFromDifferentInventory(container, slotGridFirst, ingredientReference, slotCount);

            // Didn't find ingredient items
            if (slotNum < 0)
            {
                break;
            }

            // Pick up the ingredient stack from the found slot
            leftClickSlot(gui, slotNum);

            // Successfully picked up ingredient items
            if (areStacksEqual(ingredientReference, mc.player.inventory.getItemStack()))
            {
                int filled = putSingleItemIntoSlots(gui, targetSlots, index);
                index += filled;

                if (filled < 1)
                {
                    break;
                }
            }
            // Failed to pick up the stack, break to avoid infinite loops
            // TODO: we could also "blacklist" this slot and try to continue...?
            else
            {
                break;
            }
        }

        // Return the rest of the items to the original slot
        if (slotNum >= 0 && isStackEmpty(mc.player.inventory.getItemStack()) == false)
        {
            leftClickSlot(gui, slotNum);
        }
    }

    private static boolean moveItemFromCursorToSlots(GuiContainer gui, List<Integer> slotNumbers)
    {
        Minecraft mc = Minecraft.getInstance();
        InventoryPlayer inv = mc.player.inventory;

        for (int slotNum : slotNumbers)
        {
            leftClickSlot(gui, slotNum);

            if (isStackEmpty(inv.getItemStack()))
            {
                return true;
            }
        }

        return false;
    }

    private static void moveItemsFromInventory(GuiContainer gui, int slotTo, IInventory invSrc, ItemStack stackTemplate, boolean fillStacks)
    {
        Container container = gui.inventorySlots;

        for (Slot slot : container.inventorySlots)
        {
            if (slot == null)
            {
                continue;
            }

            if (slot.inventory == invSrc && areStacksEqual(stackTemplate, slot.getStack()))
            {
                if (fillStacks)
                {
                    if (clickSlotsToMoveItems(gui, slot.slotNumber, slotTo) == false)
                    {
                        break;
                    }
                }
                else
                {
                    clickSlotsToMoveSingleItem(gui, slot.slotNumber, slotTo);
                    break;
                }
            }
        }
    }

    private static int getSlotNumberOfLargestMatchingStackFromDifferentInventory(Container container, Slot slotReference, ItemStack stackReference)
    {
        int slotNum = -1;
        int largest = 0;

        for (Slot slot : container.inventorySlots)
        {
            if (areSlotsInSameInventory(slot, slotReference) == false && slot.getHasStack() &&
                areStacksEqual(stackReference, slot.getStack()))
            {
                int stackSize = getStackSize(slot.getStack());

                if (stackSize > largest)
                {
                    slotNum = slot.slotNumber;
                    largest = stackSize;
                }
            }
        }

        return slotNum;
    }

    /**
     * Returns the slot number of the slot that has the smallest stackSize that is still equal to or larger
     * than idealSize. The slot must also NOT be in the same inventory as slotReference.
     * If an adequately large stack is not found, then the largest one is selected.
     * @param container
     * @param slotReference
     * @param stackReference
     * @return
     */
    private static int getSlotNumberOfSmallestStackFromDifferentInventory(Container container, Slot slotReference, ItemStack stackReference, int idealSize)
    {
        int slotNum = -1;
        int smallest = Integer.MAX_VALUE;

        for (Slot slot : container.inventorySlots)
        {
            if (areSlotsInSameInventory(slot, slotReference) == false && slot.getHasStack() &&
                areStacksEqual(stackReference, slot.getStack()))
            {
                int stackSize = getStackSize(slot.getStack());

                if (stackSize < smallest && stackSize >= idealSize)
                {
                    slotNum = slot.slotNumber;
                    smallest = stackSize;
                }
            }
        }

        // Didn't find an adequately sized stack, now try to find at least some items...
        if (slotNum == -1)
        {
            int largest = 0;

            for (Slot slot : container.inventorySlots)
            {
                if (areSlotsInSameInventory(slot, slotReference) == false && slot.getHasStack() &&
                    areStacksEqual(stackReference, slot.getStack()))
                {
                    int stackSize = getStackSize(slot.getStack());

                    if (stackSize > largest)
                    {
                        slotNum = slot.slotNumber;
                        largest = stackSize;
                    }
                }
            }
        }

        return slotNum;
    }

    /**
     * Return the slot numbers of slots that have items identical to stackReference.
     * If preferPartial is true, then stacks with a stackSize less that getMaxStackSize() are
     * at the beginning of the list (not ordered though) and full stacks are at the end, otherwise the reverse is true.
     * @param container
     * @param slotReference
     * @param sameInventory if true, then the returned slots are from the same inventory, if false, then from a different inventory
     * @param stackReference
     * @param preferPartial
     * @param treatHotbarAsDifferent
     * @param reverse if true, returns the slots starting from the end of the inventory
     * @return
     */
    private static List<Integer> getSlotNumbersOfMatchingStacks(
            Container container, Slot slotReference, boolean sameInventory,
            ItemStack stackReference, boolean preferPartial, boolean treatHotbarAsDifferent, boolean reverse)
    {
        List<Integer> slots = new ArrayList<Integer>(64);
        final int maxSlot = container.inventorySlots.size() - 1;
        final int increment = reverse ? -1 : 1;

        for (int i = reverse ? maxSlot : 0; i >= 0 && i <= maxSlot; i += increment)
        {
            Slot slot = container.getSlot(i);

            if (slot != null && slot.getHasStack() &&
                areSlotsInSameInventory(slot, slotReference, treatHotbarAsDifferent) == sameInventory &&
                areStacksEqual(slot.getStack(), stackReference))
            {
                if ((getStackSize(slot.getStack()) < stackReference.getMaxStackSize()) == preferPartial)
                {
                    slots.add(0, slot.slotNumber);
                }
                else
                {
                    slots.add(slot.slotNumber);
                }
            }
        }

        return slots;
    }

    private static List<Integer> getSlotNumbersOfMatchingStacks(Container container, ItemStack stackReference, boolean preferPartial)
    {
        List<Integer> slots = new ArrayList<Integer>(64);
        final int maxSlot = container.inventorySlots.size() - 1;

        for (int i = 0; i <= maxSlot; ++i)
        {
            Slot slot = container.getSlot(i);

            if (slot != null && slot.getHasStack() && areStacksEqual(slot.getStack(), stackReference))
            {
                if ((getStackSize(slot.getStack()) < stackReference.getMaxStackSize()) == preferPartial)
                {
                    slots.add(0, slot.slotNumber);
                }
                else
                {
                    slots.add(slot.slotNumber);
                }
            }
        }

        return slots;
    }

    private static List<Integer> getSlotNumbersOfEmptySlots(
            Container container, Slot slotReference, boolean sameInventory, boolean treatHotbarAsDifferent, boolean reverse)
    {
        List<Integer> slots = new ArrayList<Integer>(64);
        final int maxSlot = container.inventorySlots.size() - 1;
        final int increment = reverse ? -1 : 1;

        for (int i = reverse ? maxSlot : 0; i >= 0 && i <= maxSlot; i += increment)
        {
            Slot slot = container.getSlot(i);

            if (slot != null && slot.getHasStack() == false &&
                areSlotsInSameInventory(slot, slotReference, treatHotbarAsDifferent) == sameInventory)
            {
                slots.add(slot.slotNumber);
            }
        }

        return slots;
    }

    private static List<Integer> getSlotNumbersOfEmptySlotsInPlayerInventory(Container container, boolean reverse)
    {
        List<Integer> slots = new ArrayList<Integer>(64);
        final int maxSlot = container.inventorySlots.size() - 1;
        final int increment = reverse ? -1 : 1;

        for (int i = reverse ? maxSlot : 0; i >= 0 && i <= maxSlot; i += increment)
        {
            Slot slot = container.getSlot(i);

            if (slot != null && (slot.inventory instanceof InventoryPlayer) && slot.getHasStack() == false)
            {
                slots.add(slot.slotNumber);
            }
        }

        return slots;
    }

    public static boolean areStacksEqual(ItemStack stack1, ItemStack stack2)
    {
        return ItemStack.areItemsEqual(stack1, stack2) && ItemStack.areItemStackTagsEqual(stack1, stack2);
    }

    private static boolean areSlotsInSameInventory(Slot slot1, Slot slot2)
    {
        return areSlotsInSameInventory(slot1, slot2, false);
    }

    private static boolean areSlotsInSameInventory(Slot slot1, Slot slot2, boolean treatHotbarAsDifferent)
    {
        if (slot1.inventory == slot2.inventory)
        {
            if (treatHotbarAsDifferent && slot1.inventory instanceof InventoryPlayer)
            {
                int index1 = AccessorUtils.getSlotIndex(slot1);
                int index2 = AccessorUtils.getSlotIndex(slot2);
                // Don't ever treat the offhand slot as a different inventory
                return index1 == 40 || index2 == 40 || (index1 < 9) == (index2 < 9);
            }

            return true;
        }

        return false;
    }

    private static ItemStack[] getOriginalStacks(Container container)
    {
        ItemStack[] originalStacks = new ItemStack[container.inventorySlots.size()];

        for (int i = 0; i < originalStacks.length; i++)
        {
            originalStacks[i] = container.inventorySlots.get(i).getStack().copy();
        }

        return originalStacks;
    }

    private static void restoreOriginalStacks(Container container, ItemStack[] originalStacks)
    {
        for (int i = 0; i < originalStacks.length; i++)
        {
            ItemStack stackSlot = container.getSlot(i).getStack();

            if (areStacksEqual(stackSlot, originalStacks[i]) == false ||
                (isStackEmpty(stackSlot) == false && getStackSize(stackSlot) != getStackSize(originalStacks[i])))
            {
                container.putStackInSlot(i, originalStacks[i]);
            }
        }
    }

    private static int getTargetSlot(Container container, ItemStack[] originalStacks)
    {
        List<Slot> slots = container.inventorySlots;

        for (int i = 0; i < originalStacks.length; i++)
        {
            ItemStack stackOrig = originalStacks[i];
            ItemStack stackNew = slots.get(i).getStack();

            if ((isStackEmpty(stackOrig) && isStackEmpty(stackNew) == false) ||
               (isStackEmpty(stackOrig) == false && isStackEmpty(stackNew) == false &&
               getStackSize(stackNew) == (getStackSize(stackOrig) + 1)))
            {
                return i;
            }
        }

        return -1;
    }

    /*
    private void clickSlotsToMoveItems(Slot slot, GuiContainer gui, boolean matchingOnly, boolean toOtherInventory)
    {
        for (Slot slotTmp : gui.inventorySlots.inventorySlots)
        {
            if (slotTmp.slotNumber != slot.slotNumber && areSlotsInSameInventory(slotTmp, slot) == toOtherInventory &&
                slotTmp.getHasStack() && (matchingOnly == false || areStacksEqual(slot.getStack(), slotTmp.getStack())))
            {
                this.clickSlotsToMoveItemsFromSlot(slotTmp, gui, toOtherInventory);
                return;
            }
        }

        // Move the hovered-over slot's stack last
        if (toOtherInventory)
        {
            this.clickSlotsToMoveItemsFromSlot(slot, gui, toOtherInventory);
        }
    }
    */

    private static void clickSlotsToMoveItemsFromSlot(Slot slotFrom, GuiContainer gui, boolean toOtherInventory)
    {
        Minecraft mc = Minecraft.getInstance();
        EntityPlayer player = mc.player;
        // Left click to pick up the found source stack
        leftClickSlot(gui, slotFrom.slotNumber);

        if (isStackEmpty(player.inventory.getItemStack()))
        {
            return;
        }

        for (Slot slotDst : gui.inventorySlots.inventorySlots)
        {
            ItemStack stackDst = slotDst.getStack();

            if (areSlotsInSameInventory(slotDst, slotFrom) != toOtherInventory &&
                (isStackEmpty(stackDst) || areStacksEqual(stackDst, player.inventory.getItemStack())))
            {
                // Left click to (try and) place items to the slot
                leftClickSlot(gui, slotDst.slotNumber);
            }

            if (isStackEmpty(player.inventory.getItemStack()))
            {
                return;
            }
        }

        // Couldn't fit the entire stack to the target inventory, return the rest of the items
        if (isStackEmpty(player.inventory.getItemStack()) == false)
        {
            leftClickSlot(gui, slotFrom.slotNumber);
        }
    }

    private static boolean clickSlotsToMoveSingleItem(GuiContainer gui, int slotFrom, int slotTo)
    {
        //System.out.println("clickSlotsToMoveSingleItem(from: " + slotFrom + ", to: " + slotTo + ")");
        Minecraft mc = Minecraft.getInstance();
        ItemStack stack = gui.inventorySlots.inventorySlots.get(slotFrom).getStack();

        if (isStackEmpty(stack))
        {
            return false;
        }

        // Click on the from-slot to take items to the cursor - if there is more than one item in the from-slot,
        // right click on it, otherwise left click.
        if (getStackSize(stack) > 1)
        {
            rightClickSlot(gui, slotFrom);
        }
        else
        {
            leftClickSlot(gui, slotFrom);
        }

        // Right click on the target slot to put one item to it
        rightClickSlot(gui, slotTo);

        // If there are items left in the cursor, then return them back to the original slot
        if (isStackEmpty(mc.player.inventory.getItemStack()) == false)
        {
            // Left click again on the from-slot to return the rest of the items to it
            leftClickSlot(gui, slotFrom);
        }

        return true;
    }

    private static boolean clickSlotsToMoveSingleItemByShiftClick(GuiContainer gui, int slotFrom)
    {
        Minecraft mc = Minecraft.getInstance();
        Slot slot = gui.inventorySlots.inventorySlots.get(slotFrom);
        ItemStack stack = slot.getStack();

        if (isStackEmpty(stack))
        {
            return false;
        }

        if (getStackSize(stack) > 1)
        {
            // Left click on the from-slot to take all the items to the cursor
            leftClickSlot(gui, slotFrom);

            // Still items left in the slot, put the stack back and abort
            if (slot.getHasStack())
            {
                leftClickSlot(gui, slotFrom);
                return false;
            }
            else
            {
                // Right click one item back to the slot
                rightClickSlot(gui, slotFrom);
            }
        }

        // ... and then shift-click on the slot
        shiftClickSlot(gui, slotFrom);

        if (isStackEmpty(mc.player.inventory.getItemStack()) == false)
        {
            // ... and then return the rest of the items
            leftClickSlot(gui, slotFrom);
        }

        return true;
    }

    /**
     * Try move items from slotFrom to slotTo
     * @return true if at least some items were moved
     */
    private static boolean clickSlotsToMoveItems(GuiContainer gui, int slotFrom, int slotTo)
    {
        Minecraft mc = Minecraft.getInstance();
        EntityPlayer player = mc.player;
        //System.out.println("clickSlotsToMoveItems(from: " + slotFrom + ", to: " + slotTo + ")");

        // Left click to take items
        leftClickSlot(gui, slotFrom);

        // Couldn't take the items, bail out now
        if (isStackEmpty(player.inventory.getItemStack()))
        {
            return false;
        }

        boolean ret = true;
        int size = getStackSize(player.inventory.getItemStack());

        // Left click on the target slot to put the items to it
        leftClickSlot(gui, slotTo);

        // If there are items left in the cursor, then return them back to the original slot
        if (isStackEmpty(player.inventory.getItemStack()) == false)
        {
            ret = getStackSize(player.inventory.getItemStack()) != size;

            // Left click again on the from-slot to return the rest of the items to it
            leftClickSlot(gui, slotFrom);
        }

        return ret;
    }

    public static void dropStacksUntilEmpty(GuiContainer gui, int slotNum)
    {
        if (slotNum >= 0 && slotNum < gui.inventorySlots.inventorySlots.size())
        {
            Slot slot = gui.inventorySlots.getSlot(slotNum);
            int failsafe = 64;

            while (failsafe-- > 0 && slot.getHasStack())
            {
                dropStack(gui, slotNum);
            }
        }
    }

    public static void dropStacksWhileHasItem(GuiContainer gui, int slotNum, ItemStack stackReference)
    {
        if (slotNum >= 0 && slotNum < gui.inventorySlots.inventorySlots.size())
        {
            Slot slot = gui.inventorySlots.getSlot(slotNum);
            int failsafe = 256;

            while (failsafe-- > 0 && areStacksEqual(slot.getStack(), stackReference))
            {
                dropStack(gui, slotNum);
            }
        }
    }

    private static boolean shiftClickSlotWithCheck(GuiContainer gui, int slotNum)
    {
        Slot slot = gui.inventorySlots.getSlot(slotNum);

        if (slot == null || slot.getHasStack() == false)
        {
            return false;
        }

        int sizeOrig = getStackSize(slot.getStack());
        shiftClickSlot(gui, slotNum);

        return slot.getHasStack() == false || getStackSize(slot.getStack()) != sizeOrig;
    }

    public static boolean tryMoveItemsVertically(GuiContainer gui, Slot slot, boolean moveUp, MoveAmount amount)
    {
        Minecraft mc = Minecraft.getInstance();

        // We require an empty cursor
        if (slot == null || isStackEmpty(mc.player.inventory.getItemStack()) == false)
        {
            return false;
        }

        List<Integer> slots = getVerticallyFurthestSuitableSlotsForStackInSlot(gui.inventorySlots, slot, moveUp);

        if (slots.isEmpty())
        {
            return false;
        }

        if (amount == MoveAmount.FULL_STACKS)
        {
            moveStackToSlots(gui, slot, slots, false);
        }
        else if (amount == MoveAmount.MOVE_ONE)
        {
            moveOneItemToFirstValidSlot(gui, slot, slots);
        }
        else if (amount == MoveAmount.LEAVE_ONE)
        {
            moveStackToSlots(gui, slot, slots, true);
        }
        else if (amount == MoveAmount.ALL_MATCHING)
        {
            moveMatchingStacksToSlots(gui, slot, moveUp);
        }

        return true;
    }

    private static void moveMatchingStacksToSlots(GuiContainer gui, Slot slot, boolean moveUp)
    {
        List<Integer> matchingSlots = getSlotNumbersOfMatchingStacks(gui.inventorySlots, slot, true, slot.getStack(), true, true, false);
        List<Integer> targetSlots = getSlotNumbersOfEmptySlots(gui.inventorySlots, slot, false, true, false);
        targetSlots.addAll(getSlotNumbersOfEmptySlots(gui.inventorySlots, slot, true, true, false));
        targetSlots.addAll(matchingSlots);

        Collections.sort(matchingSlots, new SlotVerticalSorterSlotNumbers(gui.inventorySlots, ! moveUp));
        Collections.sort(targetSlots, new SlotVerticalSorterSlotNumbers(gui.inventorySlots, moveUp));

        for (int i = 0; i < matchingSlots.size(); ++i)
        {
            int srcSlotNum = matchingSlots.get(i).intValue();
            Slot srcSlot = gui.inventorySlots.getSlot(srcSlotNum);
            Slot lastSlot = moveStackToSlots(gui, srcSlot, targetSlots, false);

            if (lastSlot == null || (lastSlot.slotNumber == srcSlot.slotNumber || (lastSlot.yPos > srcSlot.yPos) == moveUp))
            {
                return;
            }
        }
    }

    private static Slot moveStackToSlots(GuiContainer gui, Slot slotFrom, List<Integer> slotsTo, boolean leaveOne)
    {
        Minecraft mc = Minecraft.getInstance();
        InventoryPlayer inv = mc.player.inventory;
        Slot lastSlot = null;

        // Empty slot, nothing to do
        if (slotFrom.getHasStack() == false)
        {
            return null;
        }

        // Pick up the stack
        leftClickSlot(gui, slotFrom.slotNumber);

        if (leaveOne)
        {
            rightClickSlot(gui, slotFrom.slotNumber);
        }

        for (int slotNum : slotsTo)
        {
            // Empty cursor, all done here
            if (isStackEmpty(inv.getItemStack()))
            {
                break;
            }

            Slot dstSlot = gui.inventorySlots.getSlot(slotNum);

            if (dstSlot.isItemValid(inv.getItemStack()) &&
                (dstSlot.getHasStack() == false || areStacksEqual(dstSlot.getStack(), inv.getItemStack())))
            {
                leftClickSlot(gui, slotNum);
                lastSlot = dstSlot;
            }
        }

        // Return the rest of the items, if any
        if (isStackEmpty(inv.getItemStack()) == false)
        {
            leftClickSlot(gui, slotFrom.slotNumber);
        }

        return lastSlot;
    }

    private static void moveOneItemToFirstValidSlot(GuiContainer gui, Slot slotFrom, List<Integer> slotsTo)
    {
        Minecraft mc = Minecraft.getInstance();
        InventoryPlayer inv = mc.player.inventory;

        // Pick up half of the the stack
        rightClickSlot(gui, slotFrom.slotNumber);

        if (isStackEmpty(inv.getItemStack()))
        {
            return;
        }

        int sizeOrig = getStackSize(inv.getItemStack());

        for (int slotNum : slotsTo)
        {
            rightClickSlot(gui, slotNum);
            ItemStack stackCursor = inv.getItemStack();

            if (isStackEmpty(stackCursor) || getStackSize(stackCursor) != sizeOrig)
            {
                break;
            }
        }

        // Return the rest of the items, if any
        if (isStackEmpty(inv.getItemStack()) == false)
        {
            leftClickSlot(gui, slotFrom.slotNumber);
        }
    }

    private static List<Integer> getVerticallyFurthestSuitableSlotsForStackInSlot(Container container, final Slot slotIn, boolean above)
    {
        if (slotIn == null || slotIn.getHasStack() == false)
        {
            return Collections.emptyList();
        }

        List<Integer> slotNumbers = new ArrayList<>();
        ItemStack stackSlot = slotIn.getStack();

        for (Slot slotTmp : container.inventorySlots)
        {
            if (slotTmp.slotNumber != slotIn.slotNumber && slotTmp.yPos != slotIn.yPos)
            {
                if (above == slotTmp.yPos < slotIn.yPos)
                {
                    ItemStack stackTmp = slotTmp.getStack();

                    if ((isStackEmpty(stackTmp) && slotTmp.isItemValid(stackSlot)) ||
                        (areStacksEqual(stackTmp, stackSlot)) && slotTmp.getItemStackLimit(stackTmp) > getStackSize(stackTmp))
                    {
                        slotNumbers.add(slotTmp.slotNumber);
                    }
                }
            }
        }

        Collections.sort(slotNumbers, new SlotVerticalSorterSlotNumbers(container, above));

        return slotNumbers;
    }

    public static void tryClearCursor(GuiContainer gui, Minecraft mc)
    {
        if (mc.player.inventory.getCurrentItem().isEmpty() == false)
        {
            List<Integer> emptySlots = getSlotNumbersOfEmptySlotsInPlayerInventory(gui.inventorySlots, false);

            if (emptySlots.isEmpty() == false)
            {
                leftClickSlot(gui, emptySlots.get(0));
            }
            else
            {
                List<Integer> matchingSlots = getSlotNumbersOfMatchingStacks(gui.inventorySlots, mc.player.inventory.getCurrentItem(), true);

                if (matchingSlots.isEmpty() == false)
                {
                    for (int slotNum : matchingSlots)
                    {
                        Slot slot = gui.inventorySlots.getSlot(slotNum);
                        ItemStack stackSlot = slot.getStack();
                        ItemStack stackCursor = mc.player.inventory.getCurrentItem();

                        if (slot == null || areStacksEqual(stackSlot, stackCursor) == false ||
                            stackSlot.getCount() >= stackCursor.getMaxStackSize())
                        {
                            break;
                        }

                        leftClickSlot(gui, slotNum);
                    }
                }
            }
        }

        if (mc.player.inventory.getCurrentItem().isEmpty() == false)
        {
            dropItemsFromCursor(gui);
        }
    }

    public static void resetLastSlotNumber()
    {
        slotNumberLast = -1;
    }

    public static MoveAction getActiveMoveAction()
    {
        return activeMoveAction;
    }

    /*
    private static class SlotVerticalSorterSlots implements Comparator<Slot>
    {
        private final boolean topToBottom;

        public SlotVerticalSorterSlots(boolean topToBottom)
        {
            this.topToBottom = topToBottom;
        }

        @Override
        public int compare(Slot slot1, Slot slot2)
        {
            if (slot1.yPos == slot2.yPos)
            {
                return (slot1.slotNumber < slot2.slotNumber) == this.topToBottom ? -1 : 1;
            }

            return (slot1.yPos < slot2.yPos) == this.topToBottom ? -1 : 1;
        }
    }
    */

    private static class SlotVerticalSorterSlotNumbers implements Comparator<Integer>
    {
        private final Container container;
        private final boolean topToBottom;

        public SlotVerticalSorterSlotNumbers(Container container, boolean topToBottom)
        {
            this.container = container;
            this.topToBottom = topToBottom;
        }

        @Override
        public int compare(Integer slotNum1, Integer slotNum2)
        {
            Slot slot1 = this.container.getSlot(slotNum1.intValue());
            Slot slot2 = this.container.getSlot(slotNum2.intValue());

            if (slot1.yPos == slot2.yPos)
            {
                return (slot1.slotNumber < slot2.slotNumber) == this.topToBottom ? -1 : 1;
            }

            return (slot1.yPos < slot2.yPos) == this.topToBottom ? -1 : 1;
        }
    }

    public static void clickSlot(GuiContainer gui, int slotNum, int mouseButton, ClickType type)
    {
        if (slotNum >= 0 && slotNum < gui.inventorySlots.inventorySlots.size())
        {
            Slot slot = gui.inventorySlots.getSlot(slotNum);
            clickSlot(gui, slot, slotNum, mouseButton, type);
        }
        else
        {
            try
            {
                Minecraft mc = Minecraft.getInstance();
                mc.playerController.windowClick(gui.inventorySlots.windowId, slotNum, mouseButton, type, mc.player);
            }
            catch (Exception e)
            {
                ItemScroller.logger.warn("Exception while emulating a slot click: gui: '{}', slotNum: {}, mouseButton; {}, ClickType: {}",
                        gui.getClass().getName(), slotNum, mouseButton, type, e);
            }
        }
    }

    public static void clickSlot(GuiContainer gui, Slot slot, int slotNum, int mouseButton, ClickType type)
    {
        try
        {
            AccessorUtils.handleMouseClick(gui, slot, slotNum, mouseButton, type);
        }
        catch (Exception e)
        {
            ItemScroller.logger.warn("Exception while emulating a slot click: gui: '{}', slotNum: {}, mouseButton; {}, ClickType: {}",
                    gui.getClass().getName(), slotNum, mouseButton, type, e);
        }
    }

    public static void leftClickSlot(GuiContainer gui, Slot slot, int slotNumber)
    {
        clickSlot(gui, slot, slotNumber, 0, ClickType.PICKUP);
    }

    public static void rightClickSlot(GuiContainer gui, Slot slot, int slotNumber)
    {
        clickSlot(gui, slot, slotNumber, 1, ClickType.PICKUP);
    }

    public static void shiftClickSlot(GuiContainer gui, Slot slot, int slotNumber)
    {
        clickSlot(gui, slot, slotNumber, 0, ClickType.QUICK_MOVE);
    }

    public static void leftClickSlot(GuiContainer gui, int slotNum)
    {
        clickSlot(gui, slotNum, 0, ClickType.PICKUP);
    }

    public static void rightClickSlot(GuiContainer gui, int slotNum)
    {
        clickSlot(gui, slotNum, 1, ClickType.PICKUP);
    }

    public static void shiftClickSlot(GuiContainer gui, int slotNum)
    {
        clickSlot(gui, slotNum, 0, ClickType.QUICK_MOVE);
    }

    public static void dropItemsFromCursor(GuiContainer gui)
    {
        clickSlot(gui, -999, 0, ClickType.PICKUP);
    }

    public static void dropItem(GuiContainer gui, int slotNum)
    {
        clickSlot(gui, slotNum, 0, ClickType.THROW);
    }

    public static void dropStack(GuiContainer gui, int slotNum)
    {
        clickSlot(gui, slotNum, 1, ClickType.THROW);
    }

    public static void swapSlots(GuiContainer gui, int slotNum, int otherSlot)
    {
        clickSlot(gui, slotNum, 0, ClickType.SWAP);
        clickSlot(gui, otherSlot, 0, ClickType.SWAP);
        clickSlot(gui, slotNum, 0, ClickType.SWAP);
    }

    private static void dragSplitItemsIntoSlots(GuiContainer gui, List<Integer> targetSlots)
    {
        Minecraft mc = Minecraft.getInstance();
        ItemStack stackInCursor = mc.player.inventory.getItemStack();

        if (isStackEmpty(stackInCursor))
        {
            return;
        }

        if (targetSlots.size() == 1)
        {
            leftClickSlot(gui, targetSlots.get(0));
            return;
        }

        int numSlots = gui.inventorySlots.inventorySlots.size();
        int loops = targetSlots.size();

        // Start the drag
        clickSlot(gui, -999, 0, ClickType.QUICK_CRAFT);

        for (int i = 0; i < loops; i++)
        {
            int slotNum = targetSlots.get(i);

            if (slotNum >= numSlots)
            {
                break;
            }

            clickSlot(gui, targetSlots.get(i), 1, ClickType.QUICK_CRAFT);
        }

        // End the drag
        clickSlot(gui, -999, 2, ClickType.QUICK_CRAFT);
    }

    /**************************************************************
     * Compatibility code for pre-1.11 vs. 1.11+
     * Well kind of, as in make the differences minimal,
     * only requires changing these things for the ItemStack
     * related changes.
     *************************************************************/

    public static final ItemStack EMPTY_STACK = ItemStack.EMPTY;

    public static boolean isStackEmpty(ItemStack stack)
    {
        return stack.isEmpty();
    }

    public static int getStackSize(ItemStack stack)
    {
        return stack.getCount();
    }

    public static void setStackSize(ItemStack stack, int size)
    {
        stack.setCount(size);
    }
}
