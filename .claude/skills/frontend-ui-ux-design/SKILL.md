---
name: frontend-ui-ux-design
description: Creates comprehensive frontend UI/UX designs including user interfaces, design systems, component libraries, responsive layouts, and accessibility implementations. Produces wireframes, mockups, design specifications, and implementation guidelines. Use when designing user interfaces, creating design systems, building component libraries, implementing responsive designs, ensuring accessibility compliance, or when users mention UI design, UX design, interface design, design systems, user experience, or frontend design patterns.
license: MIT
metadata:
  author: Dau Quang Thanh
  version: "1.0"
  category: design
---

# Frontend UI/UX Design Skill

This skill guides you through comprehensive frontend UI/UX design, from user research and wireframing through detailed component design, implementation, and usability testing.

## Core Capabilities

When activated, this skill enables you to:

1. **User Research & Analysis**
   - Conduct user research and create personas
   - Analyze user journeys and pain points
   - Define information architecture
   - Create user flow diagrams
   - Identify usability requirements

2. **UI/UX Design**
   - Design wireframes and mockups
   - Create high-fidelity prototypes
   - Develop design systems and style guides
   - Design responsive and adaptive layouts
   - Apply design principles (visual hierarchy, spacing, typography)

3. **Component Design**
   - Design reusable component libraries
   - Create component specifications
   - Define component states and variants
   - Design atomic design systems
   - Document component usage patterns

4. **Accessibility & Inclusivity**
   - Ensure WCAG 2.1/2.2 compliance
   - Design for screen readers
   - Implement keyboard navigation
   - Design for color blindness and visual impairments
   - Create accessible forms and interactions

5. **Frontend Implementation**
   - Translate designs to HTML/CSS/JavaScript
   - Implement responsive designs
   - Optimize performance (Core Web Vitals)
   - Integrate with frontend frameworks (React, Vue, Angular)
   - Implement design tokens and theming

## UI/UX Design Process

Follow this systematic approach when designing user interfaces:

### Phase 1: Research & Discovery

1. **User Research**
   - **User Interviews**: Understand user needs, goals, and pain points
   - **Surveys**: Gather quantitative data from target audience
   - **Analytics Review**: Analyze existing usage data and metrics
   - **Competitive Analysis**: Study competitor solutions
   - **Stakeholder Interviews**: Understand business requirements

2. **User Personas**
   Create detailed personas including:
   - Demographics (age, location, occupation)
   - Goals and motivations
   - Pain points and frustrations
   - Technical proficiency
   - Device and browser preferences
   - Usage context and scenarios

3. **User Journey Mapping**
   - Map current user journeys (as-is)
   - Identify touchpoints and pain points
   - Design future user journeys (to-be)
   - Highlight emotional states throughout journey
   - Identify opportunities for improvement

4. **Information Architecture**
   - Define content structure and hierarchy
   - Create site maps and navigation structures
   - Organize content categories
   - Plan taxonomy and metadata
   - Design search and filtering strategies

### Phase 2: Ideation & Wireframing

1. **Sketching & Brainstorming**
   - Quick paper sketches (low fidelity)
   - Explore multiple design directions
   - Collaborative design sessions
   - Crazy 8's exercises
   - Design studio workshops

2. **Wireframes**
   - **Low-Fidelity Wireframes**: Basic layouts and structure
     - Focus on layout and content hierarchy
     - No colors, minimal styling
     - Use placeholder content (lorem ipsum, boxes)
   
   - **Mid-Fidelity Wireframes**: More detailed structures
     - Add actual content and copy
     - Define component types
     - Show interaction patterns
   
   - **High-Fidelity Wireframes**: Near-final layouts
     - Real content and imagery
     - Detailed interactions
     - Responsive behaviors

3. **User Flows**
   - Design task flows for key user actions
   - Define decision points and branching
   - Map error states and edge cases
   - Design happy paths and alternative paths
   - Document entry and exit points

### Phase 3: Visual Design

1. **Design System Foundation**
   
   **Color Palette**
   - Primary colors (brand identity)
   - Secondary colors (accents, CTAs)
   - Neutral colors (text, backgrounds, borders)
   - Semantic colors (success, warning, error, info)
   - Ensure sufficient contrast ratios (WCAG AA: 4.5:1 for text)
   
   **Typography**
   - Font families (primary, secondary, monospace)
   - Type scale (h1-h6, body, captions)
   - Font weights (light, regular, medium, bold)
   - Line heights and letter spacing
   - Responsive typography (fluid type scales)
   
   **Spacing System**
   - Base unit (4px, 8px common)
   - Spacing scale (0.5x, 1x, 1.5x, 2x, 3x, 4x, 6x, 8x)
   - Consistent margins and padding
   - Grid systems (12-column, CSS Grid)
   
   **Elevation & Shadows**
   - Shadow levels (0-5 for material design)
   - Elevation hierarchy
   - Focus states and overlays

2. **Component Design**
   
   Design comprehensive component library:
   
   **Basic Components**
   - Buttons (primary, secondary, tertiary, ghost, icon)
   - Form inputs (text, textarea, select, checkbox, radio)
   - Labels and helper text
   - Icons and iconography
   - Badges and tags
   - Avatars and profile images
   
   **Navigation Components**
   - Navigation bars (top, side, mobile)
   - Breadcrumbs
   - Tabs and pills
   - Pagination
   - Steppers and progress indicators
   
   **Layout Components**
   - Cards and panels
   - Modals and dialogs
   - Drawers and sidebars
   - Accordions and collapsibles
   - Dividers and separators
   
   **Feedback Components**
   - Alerts and notifications
   - Toast messages
   - Loading states (spinners, skeletons)
   - Progress bars
   - Empty states and error states
   
   **Data Display**
   - Tables and data grids
   - Lists (ordered, unordered, description)
   - Charts and graphs
   - Timelines
   - Statistics and metrics

3. **Mockups & Prototypes**
   - Create high-fidelity mockups
   - Apply brand visual identity
   - Design interactive prototypes
   - Animate transitions and micro-interactions
   - Create clickable prototypes for testing

### Phase 4: Responsive & Adaptive Design

1. **Responsive Breakpoints**
   ```
   Mobile:      320px - 767px
   Tablet:      768px - 1023px
   Desktop:     1024px - 1439px
   Large:       1440px+
   ```

2. **Mobile-First Approach**
   - Design for mobile first, then scale up
   - Prioritize essential content and features
   - Optimize for touch interactions
   - Consider thumb zones and reachability
   - Minimize data and loading requirements

3. **Responsive Patterns**
   - **Fluid Grids**: Percentage-based layouts
   - **Flexible Images**: Scale with container
   - **Media Queries**: Breakpoint-specific styles
   - **Mobile Navigation**: Hamburger menus, bottom tabs
   - **Responsive Typography**: Fluid type scales, viewport units

4. **Touch & Gesture Design**
   - Minimum touch target size (44x44px iOS, 48x48px Android)
   - Swipe gestures (delete, refresh)
   - Pull-to-refresh
   - Pinch to zoom
   - Long press actions

### Phase 5: Accessibility & Inclusivity

1. **WCAG Compliance**
   
   **Level A (Minimum)**
   - Provide text alternatives for images
   - Ensure keyboard accessibility
   - Don't rely solely on color
   - Provide clear labels
   
   **Level AA (Recommended)**
   - Contrast ratio 4.5:1 for normal text, 3:1 for large text
   - Resize text up to 200% without loss of functionality
   - Multiple ways to find content
   - Consistent navigation
   
   **Level AAA (Enhanced)**
   - Contrast ratio 7:1 for normal text, 4.5:1 for large text
   - No images of text
   - Enhanced visual presentation

2. **Screen Reader Support**
   - Semantic HTML (nav, main, article, aside)
   - ARIA labels and roles
   - Focus management
   - Skip to content links
   - Meaningful link text

3. **Keyboard Navigation**
   - All interactive elements keyboard accessible
   - Logical tab order
   - Visible focus indicators
   - Keyboard shortcuts (with discoverability)
   - Escape to close modals/menus

4. **Inclusive Design**
   - Color blindness considerations (don't rely on color alone)
   - Motor disability considerations (large click targets)
   - Cognitive disability considerations (clear language, consistent patterns)
   - Vision impairment (zoom support, high contrast)
   - Reduce motion for users with vestibular disorders

### Phase 6: Implementation Guidelines

1. **Design Tokens**
   Define tokens for:
   - Colors (semantic and base colors)
   - Spacing (margins, padding, gaps)
   - Typography (font sizes, weights, line heights)
   - Border radius, border widths
   - Shadows and elevation
   - Animation durations and easing

2. **CSS Architecture**
   - **Methodologies**: BEM, SMACSS, OOCSS, ITCSS
   - **CSS-in-JS**: Styled Components, Emotion, CSS Modules
   - **Utility-First**: Tailwind CSS, Tachyons
   - **Component Scoping**: Avoid global styles
   - **CSS Custom Properties**: Theme variables

3. **Component Implementation**
   ```jsx
   // Example: Button Component
   <Button
     variant="primary"        // primary | secondary | ghost
     size="medium"            // small | medium | large
     disabled={false}
     loading={false}
     icon={<IconName />}
     onClick={handleClick}
   >
     Button Text
   </Button>
   ```

4. **State Management**
   - Default state
   - Hover state
   - Active/pressed state
   - Focus state (keyboard)
   - Disabled state
   - Loading state
   - Error state
   - Success state

### Phase 7: Testing & Validation

1. **Usability Testing**
   - Conduct user testing sessions
   - A/B testing for design alternatives
   - First-click testing
   - Five-second test (first impressions)
   - Tree testing (information architecture)

2. **Accessibility Testing**
   - Automated testing (axe, Lighthouse, WAVE)
   - Screen reader testing (NVDA, JAWS, VoiceOver)
   - Keyboard navigation testing
   - Color contrast testing
   - Manual WCAG audit

3. **Performance Testing**
   - Lighthouse scores
   - Core Web Vitals (LCP, FID, CLS)
   - Page load time
   - Time to Interactive (TTI)
   - Bundle size analysis

4. **Cross-Browser Testing**
   - Chrome, Firefox, Safari, Edge
   - Mobile browsers (iOS Safari, Chrome Mobile)
   - Legacy browser support (if required)

## Design Principles

### 1. Visual Hierarchy
- **Size**: Larger elements draw more attention
- **Color**: Bright colors stand out, use sparingly
- **Contrast**: High contrast for important elements
- **Spacing**: White space separates and groups elements
- **Typography**: Weight and style create emphasis

### 2. Consistency
- Consistent component behavior across the application
- Consistent terminology and labels
- Consistent visual style and spacing
- Consistent interaction patterns
- Establish and follow design patterns

### 3. Simplicity
- Remove unnecessary elements
- Focus on essential features
- Progressive disclosure (show more as needed)
- Clear and concise copy
- Minimize cognitive load

### 4. Feedback & Response
- Immediate visual feedback for user actions
- Loading states for async operations
- Success/error messages
- Progress indicators for long processes
- Micro-interactions for delight

### 5. Error Prevention & Recovery
- Validate inputs inline
- Confirm destructive actions
- Provide clear error messages
- Offer suggestions to fix errors
- Allow undo for destructive actions

### 6. User Control
- Allow users to cancel operations
- Provide save drafts functionality
- Allow customization (themes, preferences)
- Give users control over notifications
- Respect user preferences (reduced motion, high contrast)

## Design Systems

### Design System Structure

```
Design System
├── Design Tokens
│   ├── Colors
│   ├── Typography
│   ├── Spacing
│   └── Other tokens
├── Foundation
│   ├── Grid System
│   ├── Breakpoints
│   └── Accessibility
├── Components
│   ├── Basic Components
│   ├── Form Components
│   ├── Navigation Components
│   └── Complex Components
├── Patterns
│   ├── Layout Patterns
│   ├── Navigation Patterns
│   └── Interaction Patterns
└── Guidelines
    ├── Usage Guidelines
    ├── Content Guidelines
    └── Accessibility Guidelines
```

### Popular Design Systems

- **Material Design** (Google): Comprehensive, mobile-first
- **Fluent Design** (Microsoft): Modern, depth and motion
- **Human Interface Guidelines** (Apple): iOS/macOS standards
- **Carbon Design** (IBM): Enterprise-focused
- **Ant Design**: Rich component library, data-heavy
- **Chakra UI**: Accessible, composable components
- **Tailwind CSS**: Utility-first framework

## Component Patterns

### Forms Best Practices

1. **Input Design**
   - Clear labels above inputs
   - Helper text below inputs
   - Inline validation (as user types or on blur)
   - Clear error messages
   - Appropriate input types (email, tel, number)

2. **Form Layout**
   - Single column for better completion rates
   - Group related fields
   - Logical field order
   - Smart defaults and autofill
   - Progress indicators for multi-step forms

3. **Validation**
   - Real-time validation (after field blur)
   - Clear error messages
   - Highlight invalid fields
   - Disable submit until valid (optional)
   - Success feedback on submission

### Modal/Dialog Patterns

1. **When to Use**
   - Require user decision
   - Display critical information
   - Capture user input without context switch
   - Confirm destructive actions

2. **Best Practices**
   - Focus management (trap focus in modal)
   - Close on overlay click or Escape key
   - Disable background scroll
   - Clear close button
   - Descriptive title
   - Primary and secondary actions

### Navigation Patterns

1. **Top Navigation**
   - Best for: 5-7 main sections
   - Global navigation
   - Always visible
   - Horizontal layout

2. **Sidebar Navigation**
   - Best for: Many navigation items
   - Hierarchical content
   - Collapsible for space
   - Vertical layout

3. **Bottom Tab Navigation (Mobile)**
   - Best for: 3-5 primary sections
   - Quick switching between sections
   - Always visible
   - Touch-optimized

4. **Hamburger Menu**
   - Use sparingly (reduces discoverability)
   - Best for: Secondary navigation
   - Mobile-friendly
   - Animate open/close

## Responsive Design Strategies

### Content Strategy
1. **Progressive Enhancement**: Start with core content, enhance for larger screens
2. **Content Priority**: Most important content first
3. **Conditional Loading**: Load additional content for larger screens
4. **Content Reflow**: Stack horizontally on mobile, side-by-side on desktop

### Layout Strategies
1. **Fluid Grid**: Flexible columns that resize
2. **Flexible Images**: Images scale with container
3. **Reflow**: Content stacks vertically on mobile
4. **Off-Canvas**: Hide secondary content off-screen on mobile
5. **Priority+**: Show high-priority items, hide others in menu

### Navigation Strategies
1. **Toggle Menu**: Hamburger menu for mobile
2. **Tab Bar**: Bottom tabs for mobile apps
3. **Responsive Menu**: Condense to dropdown on mobile
4. **Priority+**: Show important nav items, overflow to "More"

## Performance Optimization

### Frontend Performance

1. **Asset Optimization**
   - Compress images (WebP, AVIF formats)
   - Use responsive images (srcset, picture element)
   - Lazy load images and components
   - Minify CSS and JavaScript
   - Remove unused CSS (PurgeCSS)

2. **Critical Rendering Path**
   - Inline critical CSS
   - Defer non-critical JavaScript
   - Preload critical resources
   - Minimize render-blocking resources

3. **Core Web Vitals**
   - **LCP (Largest Contentful Paint)**: < 2.5s
     - Optimize images, use CDN, reduce server response time
   - **FID (First Input Delay)**: < 100ms
     - Minimize JavaScript, split code, use web workers
   - **CLS (Cumulative Layout Shift)**: < 0.1
     - Set image dimensions, reserve space for ads, avoid inserting content

4. **JavaScript Optimization**
   - Code splitting (route-based, component-based)
   - Tree shaking (remove unused code)
   - Lazy load non-critical components
   - Use React.memo, useMemo, useCallback (React)
   - Virtual scrolling for long lists

## Design Tools & Resources

### Design Tools

**UI Design**
- **Figma**: Collaborative, browser-based, prototyping
- **Sketch**: Mac-only, plugin ecosystem
- **Adobe XD**: Adobe integration, prototyping
- **Framer**: Code-based design, advanced prototyping

**Prototyping**
- **Figma**: Interactive prototypes
- **InVision**: Advanced animations
- **Principle**: Mac-only, timeline animations
- **ProtoPie**: Sensor-based interactions

**Handoff**
- **Figma**: Dev mode, inspect, export
- **Zeplin**: Design-to-development handoff
- **Avocode**: Multi-tool support

### Frontend Frameworks

**Component Libraries**
- **React**: Component-based, virtual DOM, large ecosystem
- **Vue.js**: Progressive, flexible, easy to learn
- **Angular**: Full framework, TypeScript, enterprise
- **Svelte**: Compiled, no virtual DOM, fast

**UI Frameworks**
- **Material-UI (MUI)**: React, Material Design
- **Ant Design**: React, enterprise applications
- **Chakra UI**: React, accessible components
- **Vuetify**: Vue, Material Design
- **Bootstrap**: CSS framework, responsive grid
- **Tailwind CSS**: Utility-first CSS

### CSS Frameworks

**Utility-First**
- **Tailwind CSS**: Highly customizable, design tokens
- **Tachyons**: Minimal, functional CSS

**Component-Based**
- **Bootstrap**: Grid system, pre-built components
- **Bulma**: Modern, flexbox-based
- **Foundation**: Enterprise, advanced features

**CSS-in-JS**
- **Styled Components**: React, tagged templates
- **Emotion**: Framework-agnostic, performant
- **Stitches**: Type-safe, variant API

## Output Format

When presenting UI/UX designs, structure your response as follows:

### 1. Design Brief
- Project overview and goals
- Target audience and personas
- Key user journeys
- Success metrics

### 2. Information Architecture
- Site map or app structure
- Navigation hierarchy
- Content organization

### 3. Wireframes
- Low-fidelity layouts
- Key user flows
- Content hierarchy

### 4. Visual Design
- Color palette (with hex codes)
- Typography specifications
- Spacing system
- Component designs

### 5. Component Library
- Component catalog with variants
- Component specifications
- Usage guidelines
- Code examples

### 6. Responsive Design
- Breakpoint designs (mobile, tablet, desktop)
- Responsive behaviors
- Touch interaction patterns

### 7. Accessibility Report
- WCAG compliance level
- Contrast ratios
- Keyboard navigation flow
- Screen reader considerations

### 8. Implementation Guidelines
- Design tokens (JSON/CSS variables)
- Component API specifications
- Animation specifications
- Developer handoff notes

### 9. Style Guide
- Brand guidelines
- Design principles
- Component usage rules
- Do's and don'ts

## Design Checklist

### Before Development

- [ ] User research completed and documented
- [ ] Personas created
- [ ] User journeys mapped
- [ ] Information architecture defined
- [ ] Wireframes approved by stakeholders
- [ ] Visual designs finalized
- [ ] Design system documented
- [ ] Accessibility requirements defined
- [ ] Responsive breakpoints designed
- [ ] Animations and transitions specified

### During Development

- [ ] Design tokens implemented
- [ ] Components match designs
- [ ] Responsive behaviors correct
- [ ] Accessibility requirements met
- [ ] Cross-browser compatibility verified
- [ ] Performance benchmarks met
- [ ] Animations smooth and performant

### After Launch

- [ ] Usability testing conducted
- [ ] Analytics tracking implemented
- [ ] A/B tests running (if applicable)
- [ ] Feedback collection mechanism in place
- [ ] Performance monitoring active
- [ ] Accessibility audit completed

## Examples

### Example 1: E-Commerce Product Page

**User Journey**: Browse → View Product → Add to Cart → Checkout

**Key Components**:
- Product image gallery (with zoom)
- Product title and price
- Size/color selector
- Quantity selector
- Add to cart button (prominent)
- Product description tabs
- Customer reviews
- Related products
- Breadcrumb navigation

**Responsive Considerations**:
- Mobile: Single column, sticky add-to-cart
- Desktop: Two-column layout, sidebar details

**Accessibility**:
- Alt text for product images
- ARIA labels for selectors
- Keyboard navigation for gallery
- Clear focus indicators

### Example 2: Dashboard Analytics

**User Journey**: Login → View Overview → Drill into Metrics → Export

**Key Components**:
- Sidebar navigation
- Stats cards (KPIs)
- Charts (line, bar, pie)
- Data tables with filters
- Date range picker
- Export functionality

**Responsive Considerations**:
- Mobile: Collapsible sidebar, stacked cards
- Desktop: Fixed sidebar, grid layout for cards

**Accessibility**:
- Screen reader announcements for dynamic data
- Keyboard navigation for charts
- Data tables with proper headers
- Skip to content link

### Example 3: Mobile App Onboarding

**User Journey**: Open App → Welcome Screens → Sign Up → Tutorial

**Key Screens**:
1. Welcome splash (brand introduction)
2. Features overview (3-4 screens, swipeable)
3. Sign up form (minimal fields)
4. Permissions request (with rationale)
5. Quick tutorial (interactive)

**Design Patterns**:
- Swipeable carousel for features
- Progress indicators (dots)
- Skip button (top right)
- Clear CTA buttons
- Celebratory success state

## Activation Guidelines

This skill should be activated when:
- Designing user interfaces from scratch
- Creating or updating design systems
- Building component libraries
- Implementing responsive designs
- Ensuring accessibility compliance
- Conducting design reviews
- Creating design documentation
- Planning frontend architecture
- Optimizing user experience
- Preparing developer handoff materials
- Designing mobile applications
- Creating marketing websites or landing pages

The skill provides the most value when given user research data, brand guidelines, and clear project requirements.
