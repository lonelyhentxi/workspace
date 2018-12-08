import { Component, OnInit } from '@angular/core';
import {DomSanitizer, SafeUrl} from '@angular/platform-browser';

@Component({
  selector: 'app-security-overview',
  template: `
    <h3>Binding innerHTML</h3>
    <p>Bound Value</p>
    <p class="e2e-inner-html-interpolated">{{htmlSnippet}}</p>
    <p>Result of binding of innerHTML:</p>
    <p class="e2e-inner-html-bound" [innerHTML]="htmlSnippet"></p>
    <h4>An untrusted URL:</h4>
    <p><a class="e2e-dangerous-url" [href]="dangerousUrl">Click me</a></p>
    <h4>A trusted URL:</h4>
    <p><a class="e2e-trusted-url" [href]="trustedUrl">Click me</a></p>
  `,
  styles: []
})
export class SecurityOverviewComponent implements OnInit {
  htmlSnippet = 'Template <script>alert("0wned")</script> <b>Syntax</b>';
  dangerousUrl: string;
  trustedUrl: SafeUrl;

  constructor(private readonly sanitizer: DomSanitizer) {
    this.dangerousUrl = 'javascript:alert("Hi there")';
    this.trustedUrl = this.sanitizer.bypassSecurityTrustUrl(this.dangerousUrl);
  }

  ngOnInit() {
  }

}
