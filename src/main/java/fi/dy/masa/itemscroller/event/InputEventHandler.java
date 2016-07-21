package fi.dy.masa.itemscroller.event;

import java.lang.reflect.Field;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.lwjgl.input.Mouse;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiMerchant;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.gui.inventory.GuiContainer;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.ClickType;
import net.minecraft.inventory.Container;
import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.Slot;
import net.minecraft.inventory.SlotMerchantResult;
import net.minecraft.item.ItemStack;
import net.minecraft.village.MerchantRecipe;
import net.minecraft.village.MerchantRecipeList;
import net.minecraftforge.client.event.GuiScreenEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.fml.relauncher.ReflectionHelper;
import net.minecraftforge.fml.relauncher.ReflectionHelper.UnableToAccessFieldException;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import net.minecraftforge.items.SlotItemHandler;
import fi.dy.masa.itemscroller.ItemScroller;
import fi.dy.masa.itemscroller.config.Configs;

@SideOnly(Side.CLIENT)
public class InputEventHandler
{
    private int lastPosX;
    private int lastPosY;
    private int slotNumberLast;
    private ItemStack[] originalStacks = new ItemStack[0];
    private final Set<Integer> draggedSlots = new HashSet<Integer>();
    private final Field fieldGuiLeft;
    private final Field fieldGuiTop;

    public InputEventHandler()
    {
        this.fieldGuiLeft = ReflectionHelper.findField(GuiContainer.class, "field_147003_i", "guiLeft");
        this.fieldGuiTop = ReflectionHelper.findField(GuiContainer.class, "field_147009_r", "guiTop");
    }

    @SubscribeEvent
    public void onMouseInputEvent(GuiScreenEvent.MouseInputEvent.Pre event)
    {
        int dWheel = Mouse.getEventDWheel();
        GuiScreen gui = event.getGui();

        if (event.getGui() instanceof GuiContainer)
        {
            boolean cancel = false;

            if (dWheel != 0)
            {
                if (Configs.enableScrollingVillager == true && event.getGui() instanceof GuiMerchant)
                {
                    cancel = this.tryMoveItemsVillager((GuiMerchant) gui, dWheel > 0);
                }
                else
                {
                    cancel = this.tryMoveItems((GuiContainer) gui, dWheel > 0);
                }
            }
            else
            {
                if (Configs.enableShiftPlaceItems && this.canShiftPlaceItems((GuiContainer) gui))
                {
                    cancel = this.shiftPlaceItems((GuiContainer) gui);
                }
                else if (Configs.enableDragMovingShiftLeft || Configs.enableDragMovingShiftRight || Configs.enableDragMovingControlLeft)
                {
                    cancel = this.dragMoveItems((GuiContainer) gui);
                }
            }

            if (cancel && event.isCancelable())
            {
                event.setCanceled(true);
            }
        }
    }

    private boolean isValidSlot(Slot slot, GuiContainer gui, boolean requireItems)
    {
        if (gui.inventorySlots == null || gui.inventorySlots.inventorySlots == null)
        {
            return false;
        }

        return slot != null && gui.inventorySlots.inventorySlots.contains(slot) == true && (requireItems == false || slot.getHasStack() == true);
    }

    private boolean canShiftPlaceItems(GuiContainer gui)
    {
        boolean eventKeyIsLeftButton = (Mouse.getEventButton() - 100) == gui.mc.gameSettings.keyBindAttack.getKeyCode();

        if (GuiScreen.isShiftKeyDown() == false || eventKeyIsLeftButton == false)
        {
            return false;
        }

        Slot slot = gui.getSlotUnderMouse();
        ItemStack stackCursor = gui.mc.thePlayer.inventory.getItemStack();

        // The target slot needs to be an empty, valid slot, and there needs to be items in the cursor
        return slot != null && stackCursor != null && this.isValidSlot(slot, gui, false) &&
               slot.getHasStack() == false && slot.isItemValid(stackCursor);
    }

    private boolean shiftPlaceItems(GuiContainer gui)
    {
        Slot slot = gui.getSlotUnderMouse();

        // Left click to place the items from the cursor to the slot
        gui.mc.playerController.windowClick(gui.inventorySlots.windowId, slot.slotNumber, 0, ClickType.PICKUP, gui.mc.thePlayer);

        this.tryMoveStacks(slot, gui, true, false, false);

        return true;
    }

    private boolean dragMoveItems(GuiContainer gui)
    {
        boolean leftButtonDown = Mouse.isButtonDown(0);
        boolean rightButtonDown = Mouse.isButtonDown(1);
        boolean isShiftDown = GuiScreen.isShiftKeyDown();
        boolean isControlDown = GuiScreen.isCtrlKeyDown();
        boolean eitherMouseButtonDown = leftButtonDown || rightButtonDown;
        boolean eventKeyIsLeftButton = (Mouse.getEventButton() - 100) == gui.mc.gameSettings.keyBindAttack.getKeyCode();
        boolean eventKeyIsRightButton = (Mouse.getEventButton() - 100) == gui.mc.gameSettings.keyBindUseItem.getKeyCode();
        boolean eventButtonState = Mouse.getEventButtonState();

        if ((isShiftDown && leftButtonDown && Configs.enableDragMovingShiftLeft == false) ||
            (isShiftDown && rightButtonDown && Configs.enableDragMovingShiftRight == false) ||
            (isControlDown && eitherMouseButtonDown && Configs.enableDragMovingControlLeft == false))
        {
            return false;
        }

        boolean leaveOneItem = leftButtonDown == false;
        boolean moveOnlyOne = isShiftDown == false;
        int mouseX = 0;
        int mouseY = 0;
        boolean cancel = false;

        try
        {
            mouseX = (Mouse.getEventX() * gui.width / gui.mc.displayWidth) - this.fieldGuiLeft.getInt(gui);
            mouseY = (gui.height - Mouse.getEventY() * gui.height / gui.mc.displayHeight - 1) - this.fieldGuiTop.getInt(gui);
        }
        catch (IllegalAccessException e)
        {
            ItemScroller.logger.warn("Failed to reflect GuiContainer#guiLeft or guiTop");
        }

        if (eventButtonState == true)
        {
            if (((eventKeyIsLeftButton || eventKeyIsRightButton) && isControlDown && Configs.enableDragMovingControlLeft) ||
                (eventKeyIsRightButton && isShiftDown && Configs.enableDragMovingShiftRight))
            {
                // Reset this or the method call won't do anything...
                this.slotNumberLast = -1;
                cancel = this.dragMoveFromSlotAtPosition(gui, mouseX, mouseY, leaveOneItem, moveOnlyOne);
            }
        }

        // Check that either mouse button is down
        if (cancel == false && (isShiftDown == true || isControlDown == true) && eitherMouseButtonDown == true)
        {
            int distX = mouseX - this.lastPosX;
            int distY = mouseY - this.lastPosY;
            int absX = Math.abs(distX);
            int absY = Math.abs(distY);

            if (absX > absY)
            {
                int inc = distX > 0 ? 1 : -1;

                for (int x = this.lastPosX; ; x += inc)
                {
                    int y = absX != 0 ? this.lastPosY + ((x - this.lastPosX) * distY / absX) : mouseY;
                    this.dragMoveFromSlotAtPosition(gui, x, y, leaveOneItem, moveOnlyOne);

                    if (x == mouseX)
                    {
                        break;
                    }
                }
            }
            else
            {
                int inc = distY > 0 ? 1 : -1;

                for (int y = this.lastPosY; ; y += inc)
                {
                    int x = absY != 0 ? this.lastPosX + ((y - this.lastPosY) * distX / absY) : mouseX;
                    this.dragMoveFromSlotAtPosition(gui, x, y, leaveOneItem, moveOnlyOne);

                    if (y == mouseY)
                    {
                        break;
                    }
                }
            }
        }

        this.lastPosX = mouseX;
        this.lastPosY = mouseY;

        // Always update the slot under the mouse.
        // This should prevent a "double click/move" when shift + left clicking on slots that have more
        // than one stack of items. (the regular slotClick() + a "drag move" from the slot that is under the mouse
        // when the left mouse button is pressed down and this code runs).
        Slot slot = this.getSlotAtPosition(gui, mouseX, mouseY);
        this.slotNumberLast = slot != null ? slot.slotNumber : -1;

        if (eitherMouseButtonDown == false)
        {
            this.draggedSlots.clear();
        }

        return cancel;
    }

    private boolean dragMoveFromSlotAtPosition(GuiContainer gui, int x, int y, boolean leaveOneItem, boolean moveOnlyOne)
    {
        Slot slot = this.getSlotAtPosition(gui, x, y);

        if (slot != null && slot.slotNumber != this.slotNumberLast)
        {
            if (this.isValidSlot(slot, gui, true) == true)
            {
                if (moveOnlyOne == true)
                {
                    if (this.draggedSlots.contains(slot.slotNumber) == false)
                    {
                        this.tryMoveSingleItemToOtherInventory(slot, gui);
                        this.draggedSlots.add(slot.slotNumber);
                    }
                }
                else
                {
                    if (this.draggedSlots.contains(slot.slotNumber) == false)
                    {
                        if (leaveOneItem == true)
                        {
                            this.tryMoveAllButOneItemToOtherInventory(slot, gui);
                        }
                        else
                        {
                            this.shiftClickSlot(gui.inventorySlots, gui.mc, slot.slotNumber);
                        }

                        this.draggedSlots.add(slot.slotNumber);
                    }
                }

                // Valid slot, cancel the event to prevent further processing (and thus transferStackInSlot)
                return true;
            }
        }

        return false;
    }

    private boolean isMouseOverSlot(Slot slot, int mouseX, int mouseY)
    {
        return this.isPointInRegion(slot.xDisplayPosition, slot.yDisplayPosition, 16, 16, mouseX, mouseY);
    }

    private boolean isPointInRegion(int left, int top, int width, int height, int pointX, int pointY)
    {
        return pointX >= left - 1 && pointX < left + width + 1 && pointY >= top - 1 && pointY < top + height + 1;
    }

    private Slot getSlotAtPosition(GuiContainer gui, int x, int y)
    {
        for (int i = 0; i < gui.inventorySlots.inventorySlots.size(); i++)
        {
            Slot slot = gui.inventorySlots.inventorySlots.get(i);

            if (this.isMouseOverSlot(slot, x, y) == true)
            {
                return slot;
            }
        }

        return null;
    }

    private boolean tryMoveItemsVillager(GuiMerchant gui, boolean moveToOtherInventory)
    {
        boolean isShiftDown = GuiContainer.isShiftKeyDown();

        Slot slot = gui.getSlotUnderMouse();
        if (slot == null)
        {
            return false;
        }

        // Only do stuff when scrolling over the recipe result slot
        if ((slot instanceof SlotMerchantResult) == false)
        {
            return this.tryMoveItems(gui, moveToOtherInventory);
        }

        if (this.isValidSlot(slot, gui, false) == false || gui.mc.thePlayer.inventory.getItemStack() != null)
        {
            return false;
        }

        if (isShiftDown == true)
        {
            // Try to fill the merchant's buy slots from the player inventory
            if (moveToOtherInventory == false)
            {
                this.tryMoveItemsToMerchantBuySlots(gui, true);
            }
            // Move items from sell slot to player inventory
            else if (slot.getHasStack() == true)
            {
                this.tryMoveStacks(slot, gui, true, true, true);
            }
            // Scrolling over an empty output slot, clear the buy slots
            else
            {
                this.tryMoveStacks(slot, gui, false, true, false);
            }
        }
        else
        {
            // Scrolling items from player inventory into merchant buy slots
            if (moveToOtherInventory == false)
            {
                this.tryMoveItemsToMerchantBuySlots(gui, false);
            }
            // Scrolling items from this slot/inventory into the other inventory
            else if (slot.getHasStack() == true)
            {
                this.tryMoveSingleItemToOtherInventory(slot, gui);
            }
        }

        return false;
    }

    private boolean tryMoveItems(GuiContainer gui, boolean moveToOtherInventory)
    {
        boolean isShiftDown = GuiContainer.isShiftKeyDown();
        boolean isCtrlDown = GuiContainer.isCtrlKeyDown();

        if ((Configs.enableScrollingSingle == false && isShiftDown == false && isCtrlDown == false) ||
            (Configs.enableScrollingStacks == false && isShiftDown == true && isCtrlDown == false) ||
            (Configs.enableScrollingMatchingStacks == false && isShiftDown == false && isCtrlDown == true) ||
            (Configs.enableMovingEverything == false && isShiftDown == true && isCtrlDown == true))
        {
            return false;
        }

        Slot slot = gui.getSlotUnderMouse();
        if (this.isValidSlot(slot, gui, true) == false || gui.mc.thePlayer.inventory.getItemStack() != null)
        {
            return false;
        }

        if ((Configs.reverseScrollDirectionSingle == true && isShiftDown == false) ||
            (Configs.reverseScrollDirectionStacks == true && isShiftDown == true))
        {
            moveToOtherInventory = ! moveToOtherInventory;
        }

        if (isShiftDown == true)
        {
            // Ctrl + Shift + scroll: move everything
            if (isCtrlDown == true)
            {
                this.tryMoveStacks(slot, gui, false, moveToOtherInventory, false);
            }
            // Shift + scroll: move one matching stack
            else
            {
                this.tryMoveStacks(slot, gui, true, moveToOtherInventory, true);
            }
        }
        // Ctrl + scroll: Move all matching stacks
        else if (isCtrlDown == true)
        {
            this.tryMoveStacks(slot, gui, true, moveToOtherInventory, false);
        }
        // No Ctrl or Shift
        else
        {
            ItemStack stack = slot.getStack();

            // Scrolling items from this slot/inventory into the other inventory
            if (moveToOtherInventory == true)
            {
                this.tryMoveSingleItemToOtherInventory(slot, gui);
            }
            // Scrolling items from the other inventory into this slot/inventory
            else if (stack.stackSize < slot.getItemStackLimit(stack))
            {
                this.tryMoveSingleItemToThisInventory(slot, gui);
            }
        }

        return false;
    }

    private void tryMoveSingleItemToOtherInventory(Slot slot, GuiContainer gui)
    {
        ItemStack stackOrig = slot.getStack();

        if (slot.canTakeStack(gui.mc.thePlayer) == false || (stackOrig.stackSize > 1 && slot.isItemValid(stackOrig) == false))
        {
            return;
        }

        Container container = gui.inventorySlots;
        ItemStack stack = stackOrig.copy();
        stack.stackSize = 1;

        this.originalStacks = this.getOriginalStacks(container);

        // Try to move the temporary single-item stack via the shift-click handler method
        slot.putStack(stack);
        container.transferStackInSlot(gui.mc.thePlayer, slot.slotNumber);

        // Successfully moved the item somewhere, now we want to check where it went
        if (slot.getHasStack() == false)
        {
            int targetSlot = this.getTargetSlot(container);

            // Found where the item went
            if (targetSlot >= 0)
            {
                // Remove the dummy item from the target slot (on the client side)
                container.inventorySlots.get(targetSlot).decrStackSize(1);

                // Restore the original stack to the slot under the cursor (on the client side)
                slot.putStack(stackOrig);

                // Do the slot clicks to actually move the items (on the server side)
                this.clickSlotsToMoveSingleItem(container, gui.mc, slot.slotNumber, targetSlot);
                return;
            }
        }

        // Restore the original stack to the slot under the cursor (on the client side)
        slot.putStack(stackOrig);
    }

    private void tryMoveAllButOneItemToOtherInventory(Slot slot, GuiContainer gui)
    {
        ItemStack stackOrig = slot.getStack();

        if (stackOrig.stackSize == 1 || slot.canTakeStack(gui.mc.thePlayer) == false || slot.isItemValid(stackOrig) == false)
        {
            return;
        }

        Container container = gui.inventorySlots;
        Slot emptySlot = findEmptySlotInSameInventory(slot, container, stackOrig);

        if (emptySlot != null)
        {
            // Take the stack by left clicking
            gui.mc.playerController.windowClick(container.windowId, slot.slotNumber, 0, ClickType.PICKUP, gui.mc.thePlayer);

            // Return one item by right clicking
            gui.mc.playerController.windowClick(container.windowId, slot.slotNumber, 1, ClickType.PICKUP, gui.mc.thePlayer);

            // Put the rest of the items into the empty slot
            gui.mc.playerController.windowClick(container.windowId, emptySlot.slotNumber, 0, ClickType.PICKUP, gui.mc.thePlayer);

            // Shift click the stack
            this.shiftClickSlot(container, gui.mc, emptySlot.slotNumber);

            if (emptySlot.getHasStack() == true)
            {
                // If items remain after the shift click, pick them up and return them to the original slot
                gui.mc.playerController.windowClick(container.windowId, emptySlot.slotNumber, 0, ClickType.PICKUP, gui.mc.thePlayer);
                gui.mc.playerController.windowClick(container.windowId, slot.slotNumber, 0, ClickType.PICKUP, gui.mc.thePlayer);
            }
        }
    }

    private void tryMoveSingleItemToThisInventory(Slot slot, GuiContainer gui)
    {
        Container container = gui.inventorySlots;
        ItemStack stackOrig = slot.getStack();

        if (slot.isItemValid(stackOrig) == false)
        {
            return;
        }

        for (Slot slotTmp : container.inventorySlots)
        {
            if (areSlotsInSameInventory(slotTmp, slot) == false &&
                slotTmp.getHasStack() == true && slotTmp.canTakeStack(gui.mc.thePlayer) == true &&
                (slotTmp.getStack().stackSize == 1 || slotTmp.isItemValid(slotTmp.getStack()) == true))
            {
                ItemStack stackTmp = slotTmp.getStack();
                if (areStacksEqual(stackTmp, stackOrig) == true)
                {
                    this.clickSlotsToMoveSingleItem(container, gui.mc, slotTmp.slotNumber, slot.slotNumber);
                    return;
                }
            }
        }

        // If we weren't able to move any items from another inventory, then try to move items
        // within the same inventory (mostly between the hotbar and the player inventory)
        for (Slot slotTmp : container.inventorySlots)
        {
            if (slotTmp.slotNumber != slot.slotNumber &&
                slotTmp.getHasStack() == true && slotTmp.canTakeStack(gui.mc.thePlayer) == true &&
                (slotTmp.getStack().stackSize == 1 || slotTmp.isItemValid(slotTmp.getStack()) == true))
            {
                ItemStack stackTmp = slotTmp.getStack();
                if (areStacksEqual(stackTmp, stackOrig) == true)
                {
                    this.clickSlotsToMoveSingleItem(container, gui.mc, slotTmp.slotNumber, slot.slotNumber);
                    return;
                }
            }
        }
    }

    private void tryMoveStacks(Slot slot, GuiContainer gui, boolean matchingOnly, boolean toOtherInventory, boolean firstOnly)
    {
        Container container = gui.inventorySlots;
        ItemStack stack = slot.getStack();

        for (Slot slotTmp : container.inventorySlots)
        {
            if (slotTmp.slotNumber != slot.slotNumber &&
                areSlotsInSameInventory(slotTmp, slot) == toOtherInventory &&
                (matchingOnly == false || areStacksEqual(stack, slotTmp.getStack()) == true))
            {
                this.shiftClickSlot(container, gui.mc, slotTmp.slotNumber);

                if (firstOnly == true)
                {
                    return;
                }
            }
        }

        // If moving to the other inventory, then move the hovered slot's stack last
        if (toOtherInventory == true)
        {
            this.shiftClickSlot(container, gui.mc, slot.slotNumber);
        }
    }

    private void tryMoveItemsToMerchantBuySlots(GuiMerchant gui, boolean fillStacks)
    {
        MerchantRecipeList list = gui.getMerchant().getRecipes(gui.mc.thePlayer);
        int index = 0;

        try
        {
            index = ReflectionHelper.getPrivateValue(GuiMerchant.class, gui, "field_147041_z", "selectedMerchantRecipe");
        }
        catch (UnableToAccessFieldException e)
        {
            ItemScroller.logger.warn("Failed to get the value of GuiMerchant.selectedMerchantRecipe");
        }

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

        if (buy1 != null)
        {
            this.fillBuySlot(gui, 0, buy1, fillStacks);
        }

        if (buy2 != null)
        {
            this.fillBuySlot(gui, 1, buy2, fillStacks);
        }
    }

    private void fillBuySlot(GuiContainer gui, int slotNum, ItemStack buyStack, boolean fillStacks)
    {
        Slot slot = gui.inventorySlots.getSlot(slotNum);

        // If there are items not matching the merchant recipe, move them out first
        if (areStacksEqual(buyStack, slot.getStack()) == false)
        {
            this.shiftClickSlot(gui.inventorySlots, gui.mc, slotNum);
        }

        this.moveItemsFromInventory(gui, slotNum, gui.mc.thePlayer.inventory, buyStack, fillStacks);
    }

    private void moveItemsFromInventory(GuiContainer gui, int slotTo, IInventory invSrc, ItemStack stackTemplate, boolean fillStacks)
    {
        Container container = gui.inventorySlots;

        for (Slot slot : container.inventorySlots)
        {
            if (slot == null)
            {
                continue;
            }

            if (slot.inventory == invSrc && areStacksEqual(stackTemplate, slot.getStack()) == true)
            {
                if (fillStacks == true)
                {
                    if (this.clickSlotsToMoveItems(container, gui.mc, slot.slotNumber, slotTo) == false)
                    {
                        break;
                    }
                }
                else
                {
                    this.clickSlotsToMoveSingleItem(container, gui.mc, slot.slotNumber, slotTo);
                    break;
                }
            }
        }
    }

    private static boolean areStacksEqual(ItemStack stack1, ItemStack stack2)
    {
        return ItemStack.areItemsEqual(stack1, stack2) && ItemStack.areItemStackTagsEqual(stack1, stack2);
    }

    private static boolean areSlotsInSameInventory(Slot slot1, Slot slot2)
    {
        if ((slot1 instanceof SlotItemHandler) && (slot2 instanceof SlotItemHandler))
        {
            return ((SlotItemHandler)slot1).getItemHandler() == ((SlotItemHandler)slot2).getItemHandler();
        }

        return slot1.inventory == slot2.inventory;
    }

    private Slot findEmptySlotInSameInventory(Slot slot, Container container, ItemStack stack)
    {
        for (Slot slotTmp : container.inventorySlots)
        {
            if (areSlotsInSameInventory(slotTmp, slot) == true &&
                slotTmp.getHasStack() == false && slotTmp.isItemValid(stack) == true)
            {
                return slotTmp;
            }
        }

        return null;
    }

    private ItemStack[] getOriginalStacks(Container container)
    {
        ItemStack[] originalStacks = new ItemStack[container.inventorySlots.size()];

        for (int i = 0; i < originalStacks.length; i++)
        {
            originalStacks[i] = ItemStack.copyItemStack(container.inventorySlots.get(i).getStack());
        }

        return originalStacks;
    }

    private int getTargetSlot(Container container)
    {
        List<Slot> slots = container.inventorySlots;

        for (int i = 0; i < this.originalStacks.length; i++)
        {
            ItemStack stackOrig = this.originalStacks[i];
            ItemStack stackNew = slots.get(i).getStack();

            if ((stackOrig == null && stackNew != null) ||
               (stackOrig != null && stackNew != null && stackNew.stackSize == (stackOrig.stackSize + 1)))
            {
                return i;
            }
        }

        return -1;
    }

    private void shiftClickSlot(Container container, Minecraft mc, int slot)
    {
        mc.playerController.windowClick(container.windowId, slot, 0, ClickType.QUICK_MOVE, mc.thePlayer);
    }

    private void clickSlotsToMoveSingleItem(Container container, Minecraft mc, int slotFrom, int slotTo)
    {
        EntityPlayer player = mc.thePlayer;
        //System.out.println("clickSlotsToMoveSingleItem(from: " + slotFrom + ", to: " + slotTo + ")");

        ItemStack stack = container.inventorySlots.get(slotFrom).getStack();
        boolean moreThanOne = stack != null && stack.stackSize > 1;

        // Right click on the from-slot to take items to the cursor
        // if there is more than one item in the from-slot, right click on it, otherwise left click
        mc.playerController.windowClick(container.windowId, slotFrom, moreThanOne == true ? 1 : 0, ClickType.PICKUP, player);

        // Right click on the target slot to put one item to it
        mc.playerController.windowClick(container.windowId, slotTo, 1, ClickType.PICKUP, player);

        // If there are items left in the cursor, then return them back to the original slot
        if (player.inventory.getItemStack() != null)
        {
            // Left click again on the from-slot to return the rest of the items to it
            mc.playerController.windowClick(container.windowId, slotFrom, 0, ClickType.PICKUP, player);
        }
    }

    /**
     * Try move items from slotFrom to slotTo
     * @return true if at least some items were moved
     */
    private boolean clickSlotsToMoveItems(Container container, Minecraft mc, int slotFrom, int slotTo)
    {
        EntityPlayer player = mc.thePlayer;
        //System.out.println("clickSlotsToMoveItems(from: " + slotFrom + ", to: " + slotTo + ")");

        // Left click to take items
        mc.playerController.windowClick(container.windowId, slotFrom, 0, ClickType.PICKUP, player);

        // Couldn't take the items, bail out now
        if (player.inventory.getItemStack() == null)
        {
            return false;
        }

        boolean ret = true;
        int size = player.inventory.getItemStack().stackSize;

        // Left click on the target slot to put the items to it
        mc.playerController.windowClick(container.windowId, slotTo, 0, ClickType.PICKUP, player);

        // If there are items left in the cursor, then return them back to the original slot
        if (player.inventory.getItemStack() != null)
        {
            ret = player.inventory.getItemStack().stackSize != size;

            // Left click again on the from-slot to return the rest of the items to it
            mc.playerController.windowClick(container.windowId, slotFrom, 0, ClickType.PICKUP, player);
        }

        return ret;
    }
}
