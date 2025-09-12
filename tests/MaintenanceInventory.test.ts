import { describe, expect, it, beforeEach } from "vitest";
import { Cl } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const address1 = accounts.get("wallet_1")!;
const address2 = accounts.get("wallet_2")!;
const deployer = accounts.get("deployer")!;

describe("MaintenanceInventory Contract Tests", () => {
  beforeEach(() => {
    // Reset simnet state before each test
    simnet.mineEmptyBlocks(1);
  });

  describe("Basic Functionality", () => {
    it("should set inventory manager successfully", () => {
      const { result } = simnet.callPublicFn(
        "MaintenanceInventory",
        "set-inventory-manager",
        [Cl.principal(address1)],
        deployer
      );
      expect(result).toBeOk(Cl.bool(true));
    });

    it("should reject unauthorized inventory manager changes", () => {
      const { result } = simnet.callPublicFn(
        "MaintenanceInventory",
        "set-inventory-manager",
        [Cl.principal(address2)],
        address1
      );
      expect(result).toBeErr(Cl.uint(500)); // err-owner-only
    });

    it("should add a new part successfully", () => {
      // First set the manager
      simnet.callPublicFn(
        "MaintenanceInventory",
        "set-inventory-manager",
        [Cl.principal(address1)],
        deployer
      );

      // Register a supplier first
      simnet.callPublicFn(
        "MaintenanceInventory",
        "register-supplier",
        [
          Cl.stringAscii("Parts Supply Co"),
          Cl.stringAscii("info@partssupply.com"),
          Cl.uint(7), // lead-time-days
          Cl.stringAscii("NET 30"),
        ],
        address1
      );

      const { result } = simnet.callPublicFn(
        "MaintenanceInventory",
        "register-inventory-part",
        [
          Cl.stringAscii("Brake Pads"),
          Cl.stringAscii("BP-2024-XL"),
          Cl.stringAscii("Brakes"),
          Cl.uint(10), // min-threshold
          Cl.uint(200), // max-capacity
          Cl.uint(2500), // unit-cost (in cents)
          Cl.uint(1), // supplier-id
          Cl.stringAscii("Warehouse A"),
        ],
        address1
      );
      expect(result).toBeOk(Cl.uint(1)); // Returns part-id
    });

    it("should retrieve part information correctly", () => {
      // First set the manager
      simnet.callPublicFn(
        "MaintenanceInventory",
        "set-inventory-manager",
        [Cl.principal(address1)],
        deployer
      );
      
      // Register a supplier first
      simnet.callPublicFn(
        "MaintenanceInventory",
        "register-supplier",
        [
          Cl.stringAscii("Filter Supply Co"),
          Cl.stringAscii("filters@supply.com"),
          Cl.uint(3), // lead-time-days
          Cl.stringAscii("NET 15"),
        ],
        address1
      );

      // Register a part
      simnet.callPublicFn(
        "MaintenanceInventory",
        "register-inventory-part",
        [
          Cl.stringAscii("Air Filter"),
          Cl.stringAscii("AF-2024-HD"),
          Cl.stringAscii("Filters"),
          Cl.uint(8), // min-threshold
          Cl.uint(150),
          Cl.uint(1800),
          Cl.uint(1),
          Cl.stringAscii("Warehouse C"),
        ],
        address1
      );

      // Then retrieve it
      const { result } = simnet.callReadOnlyFn(
        "MaintenanceInventory",
        "get-inventory-part",
        [Cl.uint(1)],
        address1
      );
      
      expect(result).toBeSome();
    });

    it("should register supplier successfully", () => {
      // First set the manager
      simnet.callPublicFn(
        "MaintenanceInventory",
        "set-inventory-manager",
        [Cl.principal(address1)],
        deployer
      );

      const { result } = simnet.callPublicFn(
        "MaintenanceInventory",
        "register-supplier",
        [
          Cl.stringAscii("AutoParts Plus"),
          Cl.stringAscii("contact@autoparts.com"),
          Cl.uint(5), // lead-time-days
          Cl.stringAscii("NET 30"),
        ],
        address1
      );
      expect(result).toBeOk(Cl.uint(1)); // Returns supplier-id
    });
  });
});
